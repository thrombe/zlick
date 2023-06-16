const std = @import("std");

const Stmt = @import("./parser.zig").Stmt;
const Expr = @import("./parser.zig").Expr;

pub const Printer = struct {
    const Self = @This();
    // TODO: no debug.print

    pub fn print_stmt(self: *Self, stmt: *Stmt) anyerror!void {
        switch (stmt.*) {
            .Print => |expr| {
                std.debug.print("print ", .{});
                try self.print_expr(expr);
            },
            .Expr => |expr| {
                try self.print_expr(expr);
            },
            .Let => |val| {
                std.debug.print("(let {s}", .{val.name});
                if (val.init_expr) |expr| {
                    std.debug.print(" = ", .{});
                    try self.print_expr(expr);
                }
                std.debug.print(")", .{});
            },
            .Assign => |val| {
                std.debug.print("({s} = ", .{val.name});
                try self.print_expr(val.expr);
                std.debug.print(")", .{});
            },
            .Block => |stmts| {
                std.debug.print("{{ (block)\n", .{});
                for (stmts) |s| {
                    try self.print_stmt(s);
                }
                std.debug.print("}}", .{});
            },
        }
        std.debug.print("\n", .{});
    }

    fn print_expr(self: *Self, expr: *Expr) anyerror!void {
        switch (expr.*) {
            .Binary => |val| {
                std.debug.print("(", .{});
                try self.print_expr(val.left);
                std.debug.print(" {s} ", .{val.operator.tok.to_string()});
                try self.print_expr(val.right);
                std.debug.print(")", .{});
            },
            .Unary => |val| {
                std.debug.print("(", .{});
                std.debug.print("{s} ", .{val.operator.tok.to_string()});
                try self.print_expr(val.oparand);
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
                try self.print_expr(e);
                std.debug.print(")", .{});
            },
            .Variable => |str| {
                std.debug.print("(var {s})", .{str});
            },
        }
    }
};
