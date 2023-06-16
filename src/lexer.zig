const std = @import("std");
const Token = @import("./parser.zig").Token;
const LigErr = @import("./main.zig").LigErr;

pub const Scanner = struct {
    const Self = @This();
    const TokenMap = std.StringHashMap(TokenType);

    var keywords: TokenMap = undefined;

    curr: usize,
    start: usize,
    line: usize,
    str: []const u8,

    pub fn new(str: []const u8, alloc: std.mem.Allocator) !Self {
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

    pub fn deinit(self: *Self) void {
        _ = self;
        keywords.deinit();
    }

    pub fn next(self: *Self) !?Token {
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

pub const TokenType = union(enum) {
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

    pub fn to_string(self: TokenType) []const u8 {
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
