const std = @import("std");

const TokenType = @import("./lexer.zig").TokenType;
const LigErr = @import("./main.zig").LigErr;
const Stmt = @import("./parser.zig").Stmt;
const Expr = @import("./parser.zig").Expr;

pub const Value = union(enum) {
    String: []const u8,
    Number: f64,
    None,
    Bool: bool,

    fn as_num(self: Value) ?f64 {
        switch (self) {
            .Number => |num| return num,
            else => return null,
        }
    }

    fn as_bool(self: Value) ?bool {
        switch (self) {
            .Bool => |b| return b,
            else => return null,
        }
    }
};

pub const Environment = struct {
    const Self = @This();
    const ValueMap = std.StringHashMap(Value);

    values: ValueMap,
    super: ?*Self,

    pub fn new(alloc: std.mem.Allocator) Self {
        return .{
            .values = ValueMap.init(alloc),
            .super = null,
        };
    }

    pub fn enclosed(self: *Self) !Self {
        var alloc = self.values.allocator;
        var env = .{ .values = ValueMap.init(alloc), .super = self };
        return env;
    }

    pub fn deinit(self: *Self) void {
        self.values.deinit();
    }

    pub fn define(self: *Self, name: []const u8, val: Value) !void {
        if (self.values.fetchRemove(name)) |kv| {
            // TODO: deinit when needed
            _ = kv;
        }
        try self.values.put(name, val);
    }

    pub fn get(self: *Self, name: []const u8) !Value {
        if (self.values.get(name)) |val| {
            return val;
        } else {
            if (self.super) |super| {
                return super.get(name);
            } else {
                return error.UndefinedVariable;
            }
        }
    }

    pub fn set(self: *Self, name: []const u8, val: Value) !void {
        if (self.values.getPtr(name)) |e| {
            // TODO: deallocate old value when required
            // _ = val;
            // _ = e;
            e.* = val;
        } else {
            if (self.super) |super| {
                return super.set(name, val);
            } else {
                return error.UndefinedVariable;
            }
        }
    }
};

pub const Interpreter = struct {
    const Self = @This();
    alloc: std.mem.Allocator,
    environment: Environment,

    pub fn new(alloc: std.mem.Allocator) Self {
        return .{
            .alloc = alloc,
            .environment = Environment.new(alloc),
        };
    }

    pub fn deinit(self: *Self) void {
        self.environment.deinit();
    }

    fn eval_expr(self: *Self, expr: *Expr) anyerror!Value {
        switch (expr.*) {
            .Binary => |val| {
                switch (val.operator.tok) {
                    .And => {
                        var v1 = try self.eval_expr(val.left);
                        if (v1.as_bool()) |b1| {
                            if (!b1) {
                                return .{ .Bool = false };
                            } else {
                                var v2 = try self.eval_expr(val.right);
                                if (v2.as_bool()) |b2| {
                                    return .{ .Bool = b2 };
                                }
                            }
                        }
                        return LigErr.ExpectedBooleanExpression;
                    },
                    .Or => {
                        var v1 = try self.eval_expr(val.left);
                        if (v1.as_bool()) |b1| {
                            if (b1) {
                                return .{ .Bool = true };
                            } else {
                                var v2 = try self.eval_expr(val.right);
                                if (v2.as_bool()) |b2| {
                                    return .{ .Bool = b2 };
                                }
                            }
                        }
                        return LigErr.ExpectedBooleanExpression;
                    },
                    else => {},
                }
                var v1 = try self.eval_expr(val.left);
                var v2 = try self.eval_expr(val.right);
                switch (val.operator.tok) {
                    .Plus => {
                        if (v1.as_num()) |n1| {
                            if (v2.as_num()) |n2| {
                                return .{ .Number = n1 + n2 };
                            }
                        }
                        return LigErr.BadAddition;
                    },
                    .Dash => {
                        if (v1.as_num()) |n1| {
                            if (v2.as_num()) |n2| {
                                return .{ .Number = n1 - n2 };
                            }
                        }
                        return LigErr.BadSubtraction;
                    },
                    .Star => {
                        if (v1.as_num()) |n1| {
                            if (v2.as_num()) |n2| {
                                return .{ .Number = n1 * n2 };
                            }
                        }
                        return LigErr.BadMultiplication;
                    },
                    .Slash => {
                        if (v1.as_num()) |n1| {
                            if (v2.as_num()) |n2| {
                                return .{ .Number = n1 / n2 };
                            }
                        }
                        return LigErr.BadDivision;
                    },
                    .Gt => {
                        if (v1.as_num()) |n1| {
                            if (v2.as_num()) |n2| {
                                return .{ .Bool = n1 > n2 };
                            }
                        }
                        return LigErr.BadComparison;
                    },
                    .Gte => {
                        if (v1.as_num()) |n1| {
                            if (v2.as_num()) |n2| {
                                return .{ .Bool = n1 >= n2 };
                            }
                        }
                        return LigErr.BadComparison;
                    },
                    .Lt => {
                        if (v1.as_num()) |n1| {
                            if (v2.as_num()) |n2| {
                                return .{ .Bool = n1 < n2 };
                            }
                        }
                        return LigErr.BadComparison;
                    },
                    .Lte => {
                        if (v1.as_num()) |n1| {
                            if (v2.as_num()) |n2| {
                                return .{ .Bool = n1 <= n2 };
                            }
                        }
                        return LigErr.BadComparison;
                    },
                    .BangEqual => {
                        // TODO: support any type in == and !=
                        if (v1.as_num()) |n1| {
                            if (v2.as_num()) |n2| {
                                return .{ .Bool = n1 != n2 };
                            }
                        }
                        return LigErr.BadComparison;
                    },
                    .DoubleEqual => {
                        if (v1.as_num()) |n1| {
                            if (v2.as_num()) |n2| {
                                return .{ .Bool = n1 == n2 };
                            }
                        }
                        return LigErr.BadComparison;
                    },
                    else => unreachable,
                }
            },
            .Unary => |val| {
                var v = try self.eval_expr(val.oparand);
                switch (val.operator.tok) {
                    .Bang => {
                        if (v.as_bool()) |b| {
                            return .{ .Bool = !b };
                        }
                        return LigErr.BadNegation;
                    },
                    .Dash => {
                        if (v.as_num()) |n| {
                            return .{ .Number = -n };
                        } else {
                            return LigErr.BadNegation;
                        }
                    },
                    else => unreachable,
                }
            },
            .Literal => |val| {
                switch (val) {
                    .String => |str| return .{ .String = str },
                    .Number => |num| return .{ .Number = try std.fmt.parseFloat(f64, num) },
                    .None => return .None,
                    .True => return .{ .Bool = true },
                    .False => return .{ .Bool = false },
                }
            },
            .Group => |val| {
                return self.eval_expr(val);
            },
            .Variable => |name| {
                return self.environment.get(name);
            },
        }
    }

    // stmt is assumed to be owned
    pub fn evaluate_stmt(self: *Self, stmt: *Stmt) !void {
        switch (stmt.*) {
            .Print => |expr| {
                var val = try self.eval_expr(expr);
                switch (val) {
                    .String => |str| {
                        std.debug.print("{s}", .{str});
                    },
                    .Number => |num| {
                        std.debug.print("{}", .{num});
                    },
                    .Bool => |b| {
                        std.debug.print("{}", .{b});
                    },
                    .None => {
                        std.debug.print("None", .{});
                    },
                }
                std.debug.print("\n", .{});
            },
            .Expr => |expr| {
                _ = try self.eval_expr(expr);
            },
            .Let => |val| {
                var value: Value = .None;
                if (val.init_expr) |e| {
                    value = try self.eval_expr(e);
                }

                try self.environment.define(val.name, value);
            },
            .Assign => |val| {
                var value = try self.eval_expr(val.expr);
                try self.environment.set(val.name, value);
            },
            .Block => |stmts| {
                var prev = self.environment;
                self.environment = try prev.enclosed();
                defer {
                    self.environment.deinit();
                    self.environment = prev;
                }

                for (stmts) |s| {
                    try self.evaluate_stmt(s);
                }
            },
            .If => |s| {
                var condition = try self.eval_expr(s.condition);
                switch (condition) {
                    .Bool => |b| {
                        if (b) {
                            try self.evaluate_stmt(s.if_block);
                        } else if (s.else_block) |blk| {
                            try self.evaluate_stmt(blk);
                        }
                    },
                    else => return error.ExpectedBooleanExpression,
                }
            },
        }
    }

    pub fn freeall_expr(self: *Self, expr: *Expr) void {
        defer self.alloc.destroy(expr);

        switch (expr.*) {
            .Binary => |val| {
                self.freeall_expr(val.left);
                self.freeall_expr(val.right);
            },
            .Unary => |val| {
                self.freeall_expr(val.oparand);
            },
            .Literal => {},
            .Group => |val| {
                self.freeall_expr(val);
            },
            .Variable => {},
        }
    }

    pub fn freeall_stmt(self: *Self, stmt: *Stmt) void {
        defer self.alloc.destroy(stmt);

        switch (stmt.*) {
            .Print => |e| {
                self.freeall_expr(e);
            },
            .Expr => |e| {
                self.freeall_expr(e);
            },
            .Let => |e| {
                if (e.init_expr) |ne| {
                    self.freeall_expr(ne);
                }
            },
            .Assign => |v| {
                self.freeall_expr(v.expr);
            },
            .Block => |stmts| {
                defer self.alloc.free(stmts);
                for (stmts) |s| {
                    self.freeall_stmt(s);
                }
            },
            .If => |v| {
                self.freeall_expr(v.condition);
                self.freeall_stmt(v.if_block);
                if (v.else_block) |b| {
                    self.freeall_stmt(b);
                }
            },
        }
    }
};
