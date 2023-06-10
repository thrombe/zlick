const std = @import("std");

// pub fn main() !void {
//     // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
//     std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

//     // stdout is for the actual output of your application, for example if you
//     // are implementing gzip, then only the compressed bytes should be sent to
//     // stdout, not any debugging messages.
//     const stdout_file = std.io.getStdOut().writer();
//     var bw = std.io.bufferedWriter(stdout_file);
//     const stdout = bw.writer();

//     try stdout.print("Run `zig build test` to run the tests.\n", .{});

//     try bw.flush(); // don't forget to flush!
// }

// test "simple test" {
//     var list = std.ArrayList(i32).init(std.testing.allocator);
//     defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
//     try list.append(42);
//     try std.testing.expectEqual(@as(i32, 42), list.pop());
// }

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

const LigErr = error{
    UnexpectedChar,
    UnterminatedString,
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

    fn run(_: *Self, code: []const u8, alloc: std.mem.Allocator) !void {
        var scanner = try Scanner.new(code, alloc);

        while (try scanner.next()) |token| {
            std.debug.print("{any}\n", .{token});
        }
    }

    fn report(self: *Self, line: usize, message: []const u8) void {
        std.log.err("[line {}] Error: {s}\n", .{ line, message });
        self.had_err = true;
    }
};

const TokenMap = std.StringHashMap(TokenType);

const Scanner = struct {
    const Self = @This();
    var keywords: TokenMap = undefined;

    curr: usize,
    start: usize,
    line: usize,
    str: []const u8,

    fn new(str: []const u8, alloc: std.mem.Allocator) !Self {
        keywords = TokenMap.init(alloc);
        try keywords.put("and", .And);
        try keywords.put("class", .Class);
        try keywords.put("else", .Else);
        try keywords.put("false", .False);
        try keywords.put("for", .For);
        try keywords.put("fn", .Fn);
        try keywords.put("if", .If);
        try keywords.put("None", .None);
        try keywords.put("or", .Or);
        try keywords.put("return", .Return);
        try keywords.put("super", .Super);
        try keywords.put("self", .Self);
        try keywords.put("true", .True);
        try keywords.put("let", .Let);
        try keywords.put("while", .While);

        try keywords.put("print", .Print);

        return .{ .start = 0, .curr = 0, .line = 1, .str = str };
    }

    fn next(self: *Self) !?Token {
        if (self.str.len <= self.curr) {
            return null;
        }
        const c = self.str[self.curr];
        self.curr += 1;

        var t: Token = undefined;
        t.line = self.line;

        switch (c) {
            '(' => t.tok = .LeftParen,
            ')' => t.tok = .RightParen,
            '{' => t.tok = .LeftBrace,
            '}' => t.tok = .RightBrace,
            ',' => t.tok = .Comma,
            '.' => t.tok = .Dot,
            '-' => t.tok = .Dash,
            '+' => t.tok = .Plus,
            ';' => t.tok = .Semicolon,
            '*' => t.tok = .Star,
            '!' => t.tok = if (self.match("=")) .BangEqual else .Bang,
            '=' => t.tok = if (self.match("=")) .DoubleEqual else .Equal,
            '<' => t.tok = if (self.match("=")) .Lte else .Lt,
            '>' => t.tok = if (self.match("=")) .Lte else .Gt,
            '/' => {
                if (self.match("/")) {
                    while (self.str.len > self.curr and self.str[self.curr] != '\n') {
                        self.curr += 1;
                    }
                    return self.next();
                } else {
                    t.tok = .Slash;
                }
            },
            ' ', '\r', '\t' => return self.next(),
            '\n' => {
                self.line += 1;
                return self.next();
            },
            '"' => {
                var len: usize = 0;
                var end = false;
                while (self.str.len > self.curr + len) {
                    if (self.str[self.curr + len] == '"') {
                        end = true;
                        break;
                    } else if (self.str[self.curr + len] == '\n') {
                        self.line += 1;
                    }
                    len += 1;
                }
                if (!end) {
                    return LigErr.UnterminatedString;
                }

                // TODO: unescaped string
                t.tok = .{ .String = self.str[self.curr .. self.curr + len] };
                self.curr += len + 1;
            },
            '0'...'9' => {
                var len: usize = 0;
                while (self.str.len > self.curr + len) {
                    switch (self.str[self.curr + len]) {
                        '0'...'9' => len += 1,
                        else => break,
                    }
                }
                if (self.str[self.curr + len] == '.') {
                    var decimal = false;
                    while (self.str.len > self.curr + len + 1) {
                        switch (self.str[self.curr + len + 1]) {
                            '0'...'9' => {
                                decimal = true;
                                len += 1;
                            },
                            else => break,
                        }
                    }
                    if (decimal) {
                        len += 1;
                    }
                }
                t.tok = .{ .Number = self.str[self.curr - 1 .. self.curr + len] };
                self.curr += len + 1;
            },
            'a'...'z', 'A'...'Z', '_' => {
                self.curr -= 1;
                var len: usize = 0;
                while (self.str.len > self.curr + len) {
                    switch (self.str[self.curr + len]) {
                        'a'...'z', 'A'...'Z', '0'...'9', '_' => {
                            len += 1;
                        },
                        else => break,
                    }
                }
                const ident = self.str[self.curr .. self.curr + len];
                t.tok = if (keywords.get(ident)) |tok| tok else .{ .Identifier = ident };
                self.curr += len + 1;
            },
            else => return LigErr.UnexpectedChar,
        }

        return t;
    }

    fn match(self: *Self, str: []const u8) bool {
        if (self.curr + str.len > self.str.len) {
            return false;
        }
        if (std.mem.eql(u8, self.str[self.curr .. self.curr + str.len], str)) {
            self.curr += str.len;
            return true;
        } else {
            return false;
        }
    }
};

const TokenType = union(enum) {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Dash,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    DoubleEqual,
    Gt,
    Gte,
    Lt,
    Lte,

    Identifier: []const u8,
    String: []const u8,
    Number: []const u8,

    And,
    Class,
    Else,
    False,
    Fn,
    For,
    If,
    None,
    Or,
    Return,
    Super,
    Self,
    True,
    Let,
    While,

    Print,

    Eof,

    fn to_string(self: TokenType) []const u8 {
        // return type + " " + lexeme + " " + literal;
        switch (self) {
            else => unreachable,
        }
    }
};

const Token = struct {
    tok: TokenType,
    line: u64,
};
