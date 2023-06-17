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
    Block: []*Stmt,
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
        if (self.match_next(&[_]TokenType{.Eof})) {
            return null;
        }
        return self.declaration() catch |e| {
            switch (e) {
                error.ExpectedSemicolon, error.UnexpectedEOF, error.ExpectedPrimaryExpression, error.ExpectedRightParen => return null,
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

    fn assignment_or_expr_stmt(self: *Self) !*Stmt {
        var expr = try self.expression();

        if (self.match_next(&[_]TokenType{.Equal})) {
            self.curr += 1;

            var right = try self.expression();
            if (!self.match_next(&[_]TokenType{.Semicolon})) {
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
                else => return error.InvalidAssignmentTarget,
            }
        } else if (self.match_next(&[_]TokenType{.Semicolon})) {
            self.curr += 1;

            var stmt = try self.alloc.create(Stmt);
            stmt.* = .{ .Expr = expr };
            return stmt;
        } else {
            return error.ExpectedSemicolon;
        }
    }

    fn declaration(self: *Self) !*Stmt {
        var stmt: anyerror!*Stmt = undefined;
        if (self.match_next(&[_]TokenType{.Let})) {
            self.curr += 1;
            stmt = try self.var_declaration();
        } else {
            stmt = try self.statement();
        }
        return stmt;
    }

    fn var_declaration(self: *Self) !*Stmt {
        if (self.match_next(&[_]TokenType{.{ .Identifier = undefined }})) {
            var tok = self.tokens[self.curr];
            self.curr += 1;
            var name: []const u8 = undefined;
            switch (tok.tok) {
                .Identifier => |str| name = str,
                else => unreachable,
            }

            var init_expr: ?*Expr = null;
            if (self.match_next(&[_]TokenType{.Equal})) {
                self.curr += 1;
                init_expr = try self.expression();
            }

            if (!self.match_next(&[_]TokenType{.Semicolon})) {
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
        if (self.match_next(&[_]TokenType{.Print})) {
            self.curr += 1;
            return try self.print_stmt();
        } else if (self.match_next(&[_]TokenType{.LeftBrace})) {
            self.curr += 1;

            var stmts = try self.block();
            var stmt = try self.alloc.create(Stmt);
            stmt.* = .{ .Block = stmts };
            return stmt;
        } else if (self.match_next(&[_]TokenType{.If})) {
            self.curr += 1;

            var condition = try self.expression();

            if (!self.match_next(&[_]TokenType{.LeftBrace})) {
                return error.ExpectedLeftBrace;
            }
            self.curr += 1;
            var stmts = try self.block();
            var if_block = try self.alloc.create(Stmt);
            if_block.* = .{ .Block = stmts };

            var stmt = try self.alloc.create(Stmt);
            errdefer self.alloc.destroy(stmt);

            if (self.match_next(&[_]TokenType{.Else})) {
                self.curr += 1;

                if (self.match_next(&[_]TokenType{.If})) {
                    var other_if = try self.statement();
                    stmt.* = .{ .If = .{ .condition = condition, .if_block = if_block, .else_block = other_if } };
                    return stmt;
                } else if (self.match_next(&[_]TokenType{.LeftBrace})) {
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
        } else {
            return try self.assignment_or_expr_stmt();
        }
    }

    fn block(self: *Self) ![]*Stmt {
        var stmts = std.ArrayList(*Stmt).init(self.alloc);
        while (!self.match_next(&[_]TokenType{ .RightBrace, .Eof })) {
            try stmts.append(try self.declaration());
        }
        if (!self.match_next(&[_]TokenType{.RightBrace})) {
            return error.UnexpectedEOF;
        }
        self.curr += 1;

        return stmts.toOwnedSlice();
    }

    fn print_stmt(self: *Self) !*Stmt {
        var val = try self.expression();
        if (!self.match_next(&[_]TokenType{.Semicolon})) {
            return error.ExpectedSemicolon;
        }
        self.curr += 1;
        var stmt = try self.alloc.create(Stmt);
        stmt.* = .{ .Print = val };
        return stmt;
    }

    // fn expr_stmt(self: *Self) !*Stmt {
    //     var val = try self.expression();
    //     if (!self.match_next(&[_]TokenType{.Semicolon})) {
    //         return error.ExpectedSemicolon;
    //     }
    //     self.curr += 1;
    //     var stmt = try self.alloc.create(Stmt);
    //     stmt.* = .{ .Expr = val };
    //     return stmt;
    // }

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
            self.curr += 1;

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
        // std.debug.print("{any} {*} {any}\n", .{ tok.tok, expr, expr });
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
//                | "(" expression ")"
//                | IDENTIFIER ;

// program        → declaration* EOF ;
//
// declaration    → varDecl | statement ;
//
// statement      → exprStmt
//                | printStmt | ifStatement
//                | block ;
//
// block          → "{" declaration* "}" ;
// exprStmt       → expression ";" ;
// ifStatemtnt    → "if" expression block ( "else" (ifStatement | block) )? ;
// printStmt      → "print" expression ";" ;
// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;

