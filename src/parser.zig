const std = @import("std");

const TokenType = @import("./lexer.zig").TokenType;
const LigErr = @import("./main.zig").LigErr;

pub const Token = struct {
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

pub const Expr = union(enum) {
    Literal: Literal,
    Group: *Expr,
    Variable: []const u8,
    Binary: struct {
        left: *Expr,
        operator: Token,
        right: *Expr,
    },
    Unary: struct {
        operator: Token,
        oparand: *Expr,
    },
    Call: struct {
        callee: *Expr,
        args: []*Expr,
        rparen: Token,
    },
    Get: struct {
        object: *Expr,
        name: []const u8,
    },
};

pub const Literal = union(enum) {
    True,
    False,
    None,
    Number: []const u8,
    String: []const u8,
};

pub const Stmt = union(enum) {
    Expr: *Expr,
    Print: *Expr,
    Let: struct {
        name: []const u8,
        init_expr: ?*Expr,
    },
    Assign: struct {
        name: []const u8,
        expr: *Expr,
    },
    If: struct {
        condition: *Expr,
        if_block: *Stmt,
        else_block: ?*Stmt,
    },
    While: struct {
        condition: *Expr,
        block: *Stmt,
    },
    For: struct {
        start: ?*Stmt,
        mid: ?*Expr,
        end: ?*Stmt,
        block: *Stmt,
    },
    Function: Function,
    Return: struct {
        ret_token: Token,
        val: ?*Expr,
    },
    Class: struct {
        name: []const u8,
        methods: []Function,
    },
    Set: struct {
        object: *Expr,
        name: []const u8,
        value: *Expr,
    },
    Block: []*Stmt,
    Break,
    Continue,

    pub const Function = struct {
        name: []const u8,
        params: [][]const u8,
        body: *Stmt,
    };
};

pub const Parser = struct {
    const Self = @This();
    tokens: []Token,
    alloc: std.mem.Allocator,
    curr: usize,

    // tokens is assumed to be an owned slice
    pub fn new(tokens: []Token, alloc: std.mem.Allocator) Self {
        return .{
            .tokens = tokens,
            .alloc = alloc,
            .curr = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.alloc.free(self.tokens);
        self.curr = 0;
    }

    // nom nom eat the character
    fn nom(self: *Self) !void {
        if (self.tokens.len > self.curr - 1) {
            self.curr += 1;
        } else {
            return LigErr.UnexpectedEOF;
        }
    }

    pub fn next_stmt(self: *Self) !?*Stmt {
        if (self.match_next(.Eof)) {
            return null;
        }
        return self.declaration() catch |e| {
            std.debug.print("{}\n", .{e});
            switch (e) {
                error.ExpectedSemicolon, error.UnexpectedEOF, error.ExpectedPrimaryExpression, error.ExpectedRightParen => return null,
                else => return e,
            }
        };
    }

    fn match_next(self: *Self, token: TokenType) bool {
        if (self.tokens.len - 1 < self.curr) {
            return false;
        } else {
            var tok = self.tokens[self.curr];
            return @as(std.meta.Tag(TokenType), tok.tok) == token;
        }
    }

    fn match_any(self: *Self, tokens: []const TokenType) bool {
        if (self.tokens.len - 1 < self.curr) {
            return false;
        } else {
            return self.tokens[self.curr].match(tokens);
        }
    }

    fn assignment_or_expr_stmt(self: *Self) !*Stmt {
        var expr = try self.expression();

        if (self.match_next(.Equal)) {
            self.curr += 1;

            var right = try self.expression();
            if (!self.match_next(.Semicolon)) {
                return error.ExpectedSemicolon;
            }
            self.curr += 1;

            switch (expr.*) {
                .Variable => |name| {
                    var stack_stmt = .{ .Assign = .{ .name = name, .expr = right } };
                    var stmt = try self.alloc.create(Stmt);
                    stmt.* = stack_stmt;
                    self.alloc.destroy(expr);
                    return stmt;
                },
                .Get => |val| {
                    var stack_stmt = .{ .Set = .{ .object = val.object, .name = val.name, .value = right } };
                    var stmt = try self.alloc.create(Stmt);
                    stmt.* = stack_stmt;
                    self.alloc.destroy(expr);
                    return stmt;
                },
                else => return error.InvalidAssignmentTarget,
            }
        } else if (self.match_next(.Semicolon)) {
            self.curr += 1;

            var stmt = try self.alloc.create(Stmt);
            stmt.* = .{ .Expr = expr };
            return stmt;
        } else {
            return error.ExpectedSemicolon;
        }
    }

    fn declaration(self: *Self) !*Stmt {
        var stmt: *Stmt = undefined;
        if (self.match_next(.Let)) {
            self.curr += 1;
            stmt = try self.var_declaration();
        } else if (self.match_next(.Fn)) {
            self.curr += 1;
            stmt = try self.function();
        } else if (self.match_next(.Class)) {
            self.curr += 1;

            if (!self.match_next(.{ .Identifier = "" })) {
                return error.ExpectedIdentifier;
            }
            var name = switch (self.tokens[self.curr].tok) {
                .Identifier => |v| v,
                else => unreachable,
            };
            self.curr += 1;
            if (!self.match_next(.LeftBrace)) {
                return error.ExpectedLeftBrace;
            }
            self.curr += 1;

            var methods = std.ArrayList(Stmt.Function).init(self.alloc);
            errdefer methods.deinit();

            while (!self.match_any(&[_]TokenType{ .RightBrace, .Eof })) {
                var fun = try self.function();
                defer self.alloc.destroy(fun);

                var func = switch (fun.*) {
                    .Function => |f| f,
                    else => unreachable,
                };
                try methods.append(func);
            }
            if (!self.match_next(.RightBrace)) {
                return error.ExpectedRightBrace;
            }
            self.curr += 1;

            stmt = try self.alloc.create(Stmt);
            stmt.* = .{ .Class = .{ .name = name, .methods = methods.toOwnedSlice() } };
        } else {
            stmt = try self.statement();
        }
        return stmt;
    }

    fn function(self: *Self) !*Stmt {
        if (!self.match_next(.{ .Identifier = "" })) {
            return error.ExpectedIdentifier;
        }
        var name = switch (self.tokens[self.curr].tok) {
            .Identifier => |name| name,
            else => unreachable,
        };
        self.curr += 1;

        if (!self.match_next(.LeftParen)) {
            return error.ExpectedLeftParen;
        }
        self.curr += 1;

        var params = std.ArrayList([]const u8).init(self.alloc);
        errdefer params.deinit();

        if (!self.match_next(.RightParen)) {
            while (true) {
                if (params.items.len >= 255) {
                    return error.TooManyArguments;
                }
                var param = try self.expression();
                switch (param.*) {
                    .Variable => |p| {
                        try params.append(p);
                        defer self.alloc.destroy(param);
                    },
                    else => return error.ExpectedParameter,
                }
                if (!self.match_next(.Comma)) {
                    break;
                }
                self.curr += 1;
                if (self.match_next(.RightParen)) {
                    break;
                }
            }
        }

        if (!self.match_next(.RightParen)) {
            return error.ExpectedRightParen;
        }
        self.curr += 1;

        if (!self.match_next(.LeftBrace)) {
            return error.ExpectedLeftBrace;
        }
        self.curr += 1;
        var body = try self.block();
        var body_block = try self.alloc.create(Stmt);
        body_block.* = .{ .Block = body };

        var stmt = try self.alloc.create(Stmt);
        stmt.* = .{ .Function = .{ .name = name, .params = params.toOwnedSlice(), .body = body_block } };
        return stmt;
    }

    fn var_declaration(self: *Self) !*Stmt {
        if (self.match_next(.{ .Identifier = undefined })) {
            var tok = self.tokens[self.curr];
            self.curr += 1;
            var name: []const u8 = undefined;
            switch (tok.tok) {
                .Identifier => |str| name = str,
                else => unreachable,
            }

            var init_expr: ?*Expr = null;
            if (self.match_next(.Equal)) {
                self.curr += 1;
                init_expr = try self.expression();
            }

            if (!self.match_next(.Semicolon)) {
                return error.ExpectedSemicolon;
            }
            self.curr += 1;

            var stmt = try self.alloc.create(Stmt);
            stmt.* = .{ .Let = .{ .name = name, .init_expr = init_expr } };
            return stmt;
        } else {
            return error.ExpectedVariableName;
        }
    }

    fn statement(self: *Self) anyerror!*Stmt {
        if (self.match_next(.Print)) {
            self.curr += 1;
            return try self.print_stmt();
        } else if (self.match_next(.LeftBrace)) {
            self.curr += 1;

            var stmts = try self.block();
            var stmt = try self.alloc.create(Stmt);
            stmt.* = .{ .Block = stmts };
            return stmt;
        } else if (self.match_next(.If)) {
            self.curr += 1;

            var condition = try self.expression();

            if (!self.match_next(.LeftBrace)) {
                return error.ExpectedLeftBrace;
            }
            self.curr += 1;
            var stmts = try self.block();
            var if_block = try self.alloc.create(Stmt);
            if_block.* = .{ .Block = stmts };

            var stmt = try self.alloc.create(Stmt);
            errdefer self.alloc.destroy(stmt);

            if (self.match_next(.Else)) {
                self.curr += 1;

                if (self.match_next(.If)) {
                    var other_if = try self.statement();
                    stmt.* = .{ .If = .{ .condition = condition, .if_block = if_block, .else_block = other_if } };
                    return stmt;
                } else if (self.match_next(.LeftBrace)) {
                    self.curr += 1;
                    stmts = try self.block();
                    var else_block = try self.alloc.create(Stmt);
                    else_block.* = .{ .Block = stmts };
                    stmt.* = .{ .If = .{ .condition = condition, .if_block = if_block, .else_block = else_block } };
                    return stmt;
                } else {
                    return error.ExpectedLeftBrace;
                }
            } else {
                stmt.* = .{ .If = .{ .condition = condition, .if_block = if_block, .else_block = null } };
                return stmt;
            }
        } else if (self.match_next(.While)) {
            self.curr += 1;

            var condition = try self.expression();

            if (!self.match_next(.LeftBrace)) {
                return error.ExpectedLeftBrace;
            }
            self.curr += 1;

            var stmts = try self.block();
            var blk = try self.alloc.create(Stmt);
            blk.* = .{ .Block = stmts };

            var while_stmt = try self.alloc.create(Stmt);
            while_stmt.* = .{ .While = .{ .condition = condition, .block = blk } };
            return while_stmt;
        } else if (self.match_next(.For)) {
            self.curr += 1;

            var start: ?*Stmt = null;
            if (self.match_next(.Let)) {
                self.curr += 1;

                start = try self.var_declaration();
            } else if (self.match_next(.Semicolon)) {
                self.curr += 1;
            } else {
                start = try self.assignment_or_expr_stmt();
            }

            var mid: ?*Expr = null;
            if (self.match_next(.Semicolon)) {
                self.curr += 1;
            } else {
                mid = try self.expression();

                if (!self.match_next(.Semicolon)) {
                    return error.ExpectedSemicolon;
                }
                self.curr += 1;
            }

            var end: ?*Stmt = null;
            if (self.match_next(.Semicolon)) {
                self.curr += 1;
            } else {
                end = try self.assignment_or_expr_stmt();
            }

            if (!self.match_next(.LeftBrace)) {
                return error.ExpectedLeftBrace;
            }
            self.curr += 1;

            var stmts = try self.block();
            var blk = try self.alloc.create(Stmt);
            blk.* = .{ .Block = stmts };

            var for_stmt = try self.alloc.create(Stmt);
            for_stmt.* = .{ .For = .{ .start = start, .mid = mid, .end = end, .block = blk } };
            return for_stmt;
        } else if (self.match_next(.Break)) {
            self.curr += 1;
            if (!self.match_next(.Semicolon)) {
                return error.ExpectedSemicolon;
            }
            self.curr += 1;

            var s = try self.alloc.create(Stmt);
            s.* = .Break;
            return s;
        } else if (self.match_next(.Continue)) {
            self.curr += 1;
            if (!self.match_next(.Semicolon)) {
                return error.ExpectedSemicolon;
            }
            self.curr += 1;

            var s = try self.alloc.create(Stmt);
            s.* = .Continue;
            return s;
        } else if (self.match_next(.Return)) {
            self.curr += 1;

            var expr: ?*Expr = null;
            if (!self.match_next(.Semicolon)) {
                expr = try self.expression();
            }
            if (!self.match_next(.Semicolon)) {
                return error.ExpectedSemicolon;
            }
            var tok = self.tokens[self.curr];
            self.curr += 1;

            var s = try self.alloc.create(Stmt);
            s.* = .{ .Return = .{ .val = expr, .ret_token = tok } };
            return s;
        } else {
            return try self.assignment_or_expr_stmt();
        }
    }

    fn block(self: *Self) ![]*Stmt {
        var stmts = std.ArrayList(*Stmt).init(self.alloc);
        while (!self.match_any(&[_]TokenType{ .RightBrace, .Eof })) {
            try stmts.append(try self.declaration());
        }
        if (!self.match_next(.RightBrace)) {
            return error.ExpectedRightBrace;
        }
        self.curr += 1;

        return stmts.toOwnedSlice();
    }

    fn print_stmt(self: *Self) !*Stmt {
        var val = try self.expression();
        if (!self.match_next(.Semicolon)) {
            return error.ExpectedSemicolon;
        }
        self.curr += 1;
        var stmt = try self.alloc.create(Stmt);
        stmt.* = .{ .Print = val };
        return stmt;
    }

    // fn expr_stmt(self: *Self) !*Stmt {
    //     var val = try self.expression();
    //     if (!self.match_next(.Semicolon)) {
    //         return error.ExpectedSemicolon;
    //     }
    //     self.curr += 1;
    //     var stmt = try self.alloc.create(Stmt);
    //     stmt.* = .{ .Expr = val };
    //     return stmt;
    // }

    fn expression(self: *Self) anyerror!*Expr {
        return try self.logic_or();
    }

    fn logic_or(self: *Self) anyerror!*Expr {
        var expr = try self.logic_and();

        while (self.match_next(.Or)) {
            var operator = self.tokens[self.curr];
            self.curr += 1;

            var right = try self.logic_and();
            var stack_expr = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = try self.alloc.create(Expr);
            expr.* = stack_expr;
        }

        return expr;
    }

    fn logic_and(self: *Self) anyerror!*Expr {
        var expr = try self.equality();

        while (self.match_next(.And)) {
            var operator = self.tokens[self.curr];
            self.curr += 1;

            var right = try self.equality();
            var stack_expr = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = try self.alloc.create(Expr);
            expr.* = stack_expr;
        }

        return expr;
    }

    fn equality(self: *Self) !*Expr {
        var left = try self.comparison();
        while (self.match_any(&[_]TokenType{ .BangEqual, .DoubleEqual })) {
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

        while (self.match_any(&[_]TokenType{ .Gt, .Gte, .Lt, .Lte })) {
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
        // std.debug.print("expr term {any} {} {any}\n", .{ expr, self.match_any(&[_]TokenType{ .Dash, .Plus }), self.tokens[self.curr] });

        while (self.match_any(&[_]TokenType{ .Dash, .Plus })) {
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

        while (self.match_any(&[_]TokenType{ .Slash, .Star })) {
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
        if (self.match_any(&[_]TokenType{ .Bang, .Dash })) {
            var operator = self.tokens[self.curr];
            self.curr += 1;

            var right = try self.unary();

            var stack_expr: Expr = .{ .Unary = .{ .operator = operator, .oparand = right } };
            var expr = try self.alloc.create(Expr);
            expr.* = stack_expr;
            return expr;
        } else {
            return try self.call();
        }
    }

    fn call(self: *Self) !*Expr {
        var expr = try self.primary();

        while (true) {
            if (self.match_next(.LeftParen)) {
                self.curr += 1;

                expr = try self.finish_call(expr);
            } else if (self.match_next(.Dot)) {
                self.curr += 1;

                if (!self.match_next(.{ .Identifier = "" })) {
                    return error.ExpectedIdentifier;
                }
                var name = switch (self.tokens[self.curr].tok) {
                    .Identifier => |n| n,
                    else => unreachable,
                };
                self.curr += 1;

                var e = .{ .Get = .{ .name = name, .object = expr } };
                expr = try self.alloc.create(Expr);
                expr.* = e;
            } else {
                break;
            }
        }

        return expr;
    }

    fn finish_call(self: *Self, callee: *Expr) !*Expr {
        var args = std.ArrayList(*Expr).init(self.alloc);
        errdefer args.deinit();

        if (!self.match_next(.RightParen)) {
            while (true) {
                if (args.items.len >= 255) {
                    return error.TooManyArguments;
                }
                try args.append(try self.expression());
                if (!self.match_next(.Comma)) {
                    break;
                }
                self.curr += 1;
                if (self.match_next(.RightParen)) {
                    break;
                }
            }
        }

        if (!self.match_next(.RightParen)) {
            return error.ExpectedRightParen;
        }
        var rparen = self.tokens[self.curr];
        self.curr += 1;

        var expr = try self.alloc.create(Expr);
        expr.* = .{ .Call = .{ .callee = callee, .args = args.toOwnedSlice(), .rparen = rparen } };
        return expr;
    }

    fn primary(self: *Self) !*Expr {
        if (self.tokens.len - 1 < self.curr) {
            self.warn(self.tokens[self.curr - 2], "expected primary expression");
            return LigErr.ExpectedPrimaryExpression;
        }
        var tok = self.tokens[self.curr];
        self.curr += 1;

        var expr = try self.alloc.create(Expr);
        errdefer self.alloc.destroy(expr);

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
            .Identifier => |name| expr.* = .{ .Variable = name },
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

// expression     → logic_or ;
// logic_or       → logic_and ( "or" logic_and )* ;
// logic_and      → equality ( "and" equality )* ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary | call ;
// call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
// arguments      → expression ( "," expression )* ","? ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")"
//                | IDENTIFIER ;
// parameters     → IDENTIFIER ( "," IDENTIFIER )* ","? ;

// program        → declaration* EOF ;
//
// declaration    → classDecl | varDecl | fnDecl | statement ;
//
// statement      → exprStmt | assignment | breakStmt | continueStmt
//                | printStmt | ifStatement | whileStament | forStatemtnt | returnStmt
//                | block ;
//
// returnStmt     → "return" expression? ";" ;
// breakStmt      → "break" ";" ;
// ContinueStmt   → "continue" ";" ;
// block          → "{" declaration* "}" ;
// exprStmt       → expression ";" ;
// assignment     → ( call "." )? IDENTIFIER ( "=" expression )? ";" ;
// ifStatemtnt    → "if" expression block ( "else" (ifStatement | block) )? ;
// whileStatement → "while" expression block ;
// forStatement   → "for" ( varDecl | exprStmt | assignment | ";" ) expression? ";" (assignment | exprStmt)? block ;
// printStmt      → "print" expression ";" ;
// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
// fnDecl         → "fn" function ;
// function       → IDENTIFIER "(" parameters? ")" block ;
// classDecl      → "class" IDENTIFIER "{" function* "}" ;

