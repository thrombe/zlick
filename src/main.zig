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
    // lexing errors
    UnexpectedChar,
    UnterminatedString,

    // parsing errors
    UnexpectedEOF,
    ExpectedPrimaryExpression,
    ExpectedRightParen,
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

        var tokens = std.ArrayList(Token).init(alloc);
        while (try scanner.next()) |token| {
            try tokens.append(token);
            // std.debug.print("{any}\n", .{token});
        }
        var parser = Parser.new(tokens.items, alloc);
        var expr = try parser.parse();
        if (expr) |e| {
            try print_expr(e);
            std.debug.print("\n", .{});
        }
        // std.debug.print("{any}\n", .{tokens.items});
        // std.debug.print("{any}\n", .{expr});
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
                if (self.str.len > self.curr + len and self.str[self.curr + len] == '.') {
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
                self.curr += len;
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
                self.curr += len;
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
            .Comma => return ",",
            .Dot => return ".",
            .Plus => return "+",
            .Dash => return "-",
            .Slash => return "/",
            .Star => return "*",
            .Bang => return "!",
            .BangEqual => return "!=",
            .DoubleEqual => return "==",
            .Equal => return "=",
            .Lt => return "<",
            .Gt => return ">",
            .Gte => return ">=",
            .Lte => return "<=",
            else => unreachable,
        }
    }
};

const Token = struct {
    tok: TokenType,
    line: u64,

    fn match(self: *Token, others: []const TokenType) bool {
        for (others) |typ| {
            // std.testing.expectEqual
            // return std.mem.allEqual(TokenType, &[_]TokenType{typ}, self.tok);
            if (@as(std.meta.Tag(TokenType), self.tok) == typ) {
                return true;
            }
            // if (self.tok == typ) {
            //     return true;
            // }
        }
        return false;
    }
};

const Expr = union(enum) { Binary: struct {
    left: *Expr,
    operator: Token,
    right: *Expr,
}, Unary: struct {
    operator: Token,
    oparand: *Expr,
}, Literal: Literal, Group: *Expr };

const Literal = union(enum) {
    True,
    False,
    None,
    Number: []const u8,
    String: []const u8,
};

const Parser = struct {
    const Self = @This();
    tokens: []Token,
    alloc: std.mem.Allocator,
    curr: usize,

    fn new(tokens: []Token, alloc: std.mem.Allocator) Self {
        return .{
            .tokens = tokens,
            .alloc = alloc,
            .curr = 0,
        };
    }

    // nom nom eat the character
    fn nom(self: *Self) !void {
        if (self.tokens.len > self.curr - 1) {
            self.curr += 1;
        } else {
            return LigErr.UnexpectedEOF;
        }
    }

    fn parse(self: *Self) !?*Expr {
        return self.expression() catch |e| {
            switch (e) {
                error.UnexpectedEOF, error.ExpectedPrimaryExpression, error.ExpectedRightParen => return null,
                else => return e,
            }
        };
    }

    fn match_next(self: *Self, tokens: []const TokenType) bool {
        if (self.tokens.len - 1 < self.curr) {
            return false;
        } else {
            return self.tokens[self.curr].match(tokens);
        }
    }

    fn expression(self: *Self) anyerror!*Expr {
        return try self.equality();
    }

    fn equality(self: *Self) !*Expr {
        var left = try self.comparison();
        while (self.match_next(&[_]TokenType{ .BangEqual, .DoubleEqual })) {
            var operator = self.tokens[self.curr];

            self.curr += 1;
            // try self.nom();

            var right = try self.comparison();
            var stack_left = .{ .Binary = .{ .left = left, .operator = operator, .right = right } };
            left = try self.alloc.create(Expr);
            left.* = stack_left;
        }

        return left;
    }

    fn comparison(self: *Self) !*Expr {
        var left = try self.term();

        while (self.match_next(&[_]TokenType{ .Gt, .Gte, .Lt, .Lte })) {
            var operator = self.tokens[self.curr];

            self.curr += 1;

            var right = try self.term();

            var stack_left = .{ .Binary = .{ .left = left, .operator = operator, .right = right } };
            left = try self.alloc.create(Expr);
            left.* = stack_left;
        }

        return left;
    }

    fn term(self: *Self) !*Expr {
        var expr = try self.factor();
        // std.debug.print("expr term {any} {} {any}\n", .{ expr, self.match_next(&[_]TokenType{ .Dash, .Plus }), self.tokens[self.curr] });

        while (self.match_next(&[_]TokenType{ .Dash, .Plus })) {
            var operator = self.tokens[self.curr];
            // std.debug.print("operator {any} \n", .{operator});

            self.curr += 1;

            var right = try self.factor();

            var stack_left = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = try self.alloc.create(Expr);
            expr.* = stack_left;
        }

        return expr;
    }

    fn factor(self: *Self) !*Expr {
        var expr = try self.unary();

        while (self.match_next(&[_]TokenType{ .Slash, .Star })) {
            var operator = self.tokens[self.curr];

            self.curr += 1;

            var right = try self.unary();

            var stack_left: Expr = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = try self.alloc.create(Expr);
            expr.* = stack_left;
        }

        return expr;
    }

    fn unary(self: *Self) !*Expr {
        if (self.match_next(&[_]TokenType{ .Bang, .Dash })) {
            var operator = self.tokens[self.curr];
            var right = try self.unary();

            var stack_expr: Expr = .{ .Unary = .{ .operator = operator, .oparand = right } };
            var expr = try self.alloc.create(Expr);
            expr.* = stack_expr;
            return expr;
        } else {
            return try self.primary();
        }
    }

    fn primary(self: *Self) !*Expr {
        if (self.tokens.len - 1 < self.curr) {
            self.warn(self.tokens[self.curr - 2], "expected primary expression");
            return LigErr.ExpectedPrimaryExpression;
        }
        // std.debug.print("primary {any}\n", .{self.tokens[self.curr]});
        var tok = self.tokens[self.curr];
        self.curr += 1;
        var expr = try self.alloc.create(Expr);
        switch (tok.tok) {
            .False => expr.* = .{ .Literal = .False },
            .True => expr.* = .{ .Literal = .True },
            .None => expr.* = .{ .Literal = .None },
            .Number => |num| expr.* = .{ .Literal = .{ .Number = num } },
            .String => |str| expr.* = .{ .Literal = .{ .String = str } },
            .LeftParen => {
                expr = try self.expression();
                if (self.tokens[self.curr].tok == .RightParen) {
                    self.curr += 1;
                } else {
                    self.warn(tok, "expected right paren");
                    return LigErr.ExpectedRightParen;
                }
                var stack_group: Expr = .{ .Group = expr };
                expr = try self.alloc.create(Expr);
                expr.* = stack_group;
            },
            else => {
                self.warn(tok, "expected primary expression");
                return LigErr.ExpectedPrimaryExpression;
            },
        }
        return expr;
    }

    fn warn(_: *Self, tok: Token, message: []const u8) void {
        std.log.warn("{} at '{any}' {s}\n", .{ tok.line, tok.tok, message });
    }

    fn synchronise(self: *Self) void {
        while (self.tokens.len > self.curr + 1) {
            switch (self.tokens[self.curr]) {
                .Class, .Fn, .Let, .For, .If, .While, .Print, .Return => {
                    return;
                },
                .Semicolon => {
                    self.curr += 1;
                    return;
                },
                else => {},
            }
            self.curr += 1;
        }
    }
};

// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;

fn print_expr(expr: *Expr) anyerror!void {
    switch (expr.*) {
        .Binary => |val| {
            std.debug.print("(", .{});
            try print_expr(val.left);
            std.debug.print(" {s} ", .{val.operator.tok.to_string()});
            try print_expr(val.right);
            std.debug.print(")", .{});
        },
        .Unary => |val| {
            std.debug.print("(", .{});
            std.debug.print("{s} ", .{val.operator.tok.to_string()});
            try print_expr(val.oparand);
            std.debug.print(")", .{});
        },
        .Literal => |val| {
            switch (val) {
                .True => std.debug.print("true", .{}),
                .False => std.debug.print("false", .{}),
                .None => std.debug.print("none", .{}),
                .String => |str| std.debug.print("{s}", .{str}),
                .Number => |num| std.debug.print("{s}", .{num}),
            }
        },
        .Group => |e| {
            std.debug.print("(group ", .{});
            try print_expr(e);
            std.debug.print(")", .{});
        },
    }
}
