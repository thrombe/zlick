const std = @import("std");

const Scanner = @import("./lexer.zig").Scanner;
const TokenType = @import("./lexer.zig").TokenType;
const Stmt = @import("./parser.zig").Stmt;
const Expr = @import("./parser.zig").Expr;
const Token = @import("./parser.zig").Token;
const Parser = @import("./parser.zig").Parser;
const Interpreter = @import("./interpreter.zig").Interpreter;
const ScopeResolver = @import("./interpreter.zig").ScopeResolver;
const Printer = @import("./printer.zig").Printer;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const stdout = std.io.getStdOut().writer();

    var lig = Lig.new();

    if (args.len > 2) {
        try stdout.print("lokx [script]\n", .{});
    } else if (args.len == 2) {
        try lig.run_file(args[1]);
    } else {
        try lig.repl();
    }
}

pub const LigErr = error{
    // lexing errors
    UnexpectedChar,
    UnterminatedString,

    // parsing errors
    UnexpectedEOF,
    ExpectedPrimaryExpression,
    ExpectedRightParen,
    ExpectedLeftParen,
    BadVarInitialiser,
    ExpectedSemicolon,
    ExpectedVariableName,
    ExpectedLeftBrace,
    ExpectedIdentifier,
    ExpectedParameter,
    BadLetBinding,
    ExpectedRightBrace,
    BadSelf,

    // runtime errors
    BadAddition,
    BadSubtraction,
    BadMultiplication,
    BadDivision,
    BadNegation,
    BadComparison,
    UndefinedVariable,
    ExpectedBooleanExpression,
    BadBreak,
    BadContinue,
    BadReturn,
    TooManyArguments,
    NotCallable,
    IncorrectNumberOfArgs,
    BadDepth,
    NotObject,
    UndefinedProperty,
    BadInitialiserReturn,
};

const Lig = struct {
    const Self = @This();
    had_err: bool,

    fn new() Self {
        return .{ .had_err = false };
    }

    fn repl(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();
        const stdin = std.io.getStdIn().reader();

        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const alloc = gpa.allocator();

        // assume users are not gonna type in strings too long
        var buff: [1024]u8 = undefined;
        while (true) {
            try stdout.print("> ", .{});

            if (stdin.readUntilDelimiterOrEof(&buff, '\n') catch null) |line| {
                try self.run(line, alloc);
                self.had_err = false;
            } else {
                try stdout.print("\n", .{});
                break;
            }
        }
    }

    fn run_file(self: *Self, fp: []const u8) !void {
        var f = try std.fs.cwd().openFile(fp, .{});
        defer f.close();

        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        const alloc = gpa.allocator();
        // OOF: idk why it returns a bool
        defer _ = gpa.deinit();

        var str = try f.readToEndAlloc(alloc, 10_000_000);
        defer alloc.free(str);

        // std.debug.print("{s}\n", .{str});
        try self.run(str, alloc);

        if (self.had_err) {
            std.os.exit(65);
        }
    }

    fn run(self: *Self, code: []const u8, alloc: std.mem.Allocator) !void {
        var scanner = try Scanner.new(code, alloc);
        defer scanner.deinit();

        var tokens = std.ArrayList(Token).init(alloc);
        while (try scanner.next()) |token| {
            try tokens.append(token);
            // std.debug.print("{any}\n", .{token});
        }
        try tokens.append(.{ .tok = .Eof, .line = scanner.line });

        var parser = Parser.new(tokens.toOwnedSlice(), alloc);
        defer parser.deinit();

        var printer = Printer{};
        defer printer.deinit();

        var interpreter = try Interpreter.new(alloc);
        defer interpreter.deinit();

        var resolver = ScopeResolver.new(alloc, &interpreter);
        defer resolver.deinit();

        var temp = std.ArrayList(*Stmt).init(alloc);
        defer {
            for (temp.items) |s| {
                interpreter.freeall_stmt(s);
            }
            temp.deinit();
        }

        while (try parser.next_stmt()) |s| {
            try temp.append(s);

            // try printer.print_stmt(s);

            resolver.resolve_stmt(s) catch |err| {
                std.debug.print("{}\n", .{err});
                self.had_err = true;
                continue;
            };

            if (self.had_err) {
                continue;
            }

            var r = interpreter.evaluate_stmt(s);

            if (r) |res| {
                switch (res) {
                    .Void => {},
                    .Continue => return error.BadContinue,
                    .Break => return error.BadBreak,
                    .Return => return error.BadReturn,
                }
            } else |err| {
                std.debug.print("{}\n", .{err});
                self.had_err = true;
            }
        }
    }

    fn report(self: *Self, line: usize, message: []const u8) void {
        std.log.err("[line {}] Error: {s}\n", .{ line, message });
        self.had_err = true;
    }
};
