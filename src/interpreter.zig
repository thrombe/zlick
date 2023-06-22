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
    Callable: Callable,

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

    // fn deinit(self: *Value, _: std.mem.Allocator) void {
    //     switch (self.*) {
    //         .Callable => |*c| {
    //             c.deinit();
    //         },
    //         else => {},
    //     }
    // }
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

    pub fn enclosed(self: *Self) !*Self {
        var alloc = self.values.allocator;
        var env = try alloc.create(Self);
        env.* = .{ .values = ValueMap.init(alloc), .super = self };
        return env;
    }

    pub fn deinit(self: *Self) void {
        var alloc = self.values.allocator;
        // var iter = self.values.iterator();
        // while (iter.next()) |v| {
        //     v.value_ptr.deinit(alloc);
        // }
        self.values.deinit();
        alloc.destroy(self);
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

pub const StmtResult = union(enum) {
    Break,
    Continue,
    Return: Value,
    Void,

    fn is_void(self: *StmtResult) bool {
        switch (self.*) {
            .Void => return true,
            else => return false,
        }
    }
};

// - [vtable abstraction of some kind · Issue #130](https://github.com/ziglang/zig/issues/130)
// - [zig/lib/std/heap/general_purpose_allocator.zig](https://github.com/ziglang/zig/blob/master/lib/std/heap/general_purpose_allocator.zig)
// - [zig/lib/std/mem/Allocator.zig](https://github.com/ziglang/zig/blob/master/lib/std/mem/Allocator.zig)
const Callable = struct {
    const Self = @This();
    pub const TypeArityFn = *const fn (self: *anyopaque) u8;
    pub const TypeCallFn = *const fn (self: *anyopaque, interpreter: *Interpreter, args: []Value) anyerror!Value;
    pub const TypeDeinitFn = *const fn (self: *anyopaque) void;

    self: *anyopaque,
    vtable: *const struct {
        arityFn: TypeArityFn,
        callFn: TypeCallFn,
        deinitFn: TypeDeinitFn,
    },

    pub fn arity(self: *Self) u8 {
        // fn alloc(ctx: *anyopaque, len: usize, log2_ptr_align: u8, ret_addr: usize) ?[*]u8 {
        //     const self = @ptrCast(*Self, @alignCast(@alignOf(Self), ctx));
        return self.vtable.arityFn(self.self);
    }
    pub fn call(self: *Self, interpreter: *Interpreter, args: []Value) anyerror!Value {
        return self.vtable.callFn(self.self, interpreter, args);
    }

    pub fn deinit(self: *Self) void {
        self.vtable.deinitFn(self.self);
    }

    pub fn new(val: anytype) Self {
        const Ptr = @TypeOf(val);
        const ptr_info = @typeInfo(Ptr);

        // /usr/lib/zig/std/builtin.zig
        std.debug.assert(ptr_info == .Pointer);
        const T = switch (ptr_info) {
            .Pointer => |info| info.child,
            else => unreachable,
        };
        return .{
            .self = val,
            .vtable = &.{
                .arityFn = @ptrCast(Callable.TypeArityFn, &T.arity),
                .callFn = @ptrCast(Callable.TypeCallFn, &T.call),
                .deinitFn = @ptrCast(Callable.TypeDeinitFn, &T.deinit),
            },
        };
    }
};

const Deallocatable = struct {
    const Self = @This();
    pub const TypeDeinitFn = *const fn (self: *anyopaque) void;

    self: *anyopaque,
    deinitFn: TypeDeinitFn,

    pub fn new(val: anytype) Self {
        const Ptr = @TypeOf(val);
        const ptr_info = @typeInfo(Ptr);

        // /usr/lib/zig/std/builtin.zig
        std.debug.assert(ptr_info == .Pointer);
        const T = switch (ptr_info) {
            .Pointer => |info| info.child,
            else => unreachable,
        };

        return .{
            .self = val,
            .deinitFn = @ptrCast(Self.TypeDeinitFn, &T.deinit),
        };
    }

    pub fn deinit(self: *Self) void {
        self.deinitFn(self.self);
    }
};

const Clock = struct {
    const Self = @This();
    alloc: std.mem.Allocator,

    fn arity(_: *Self) u8 {
        return 0;
    }
    fn call(_: *Self, _: *Interpreter, _: []Value) anyerror!Value {
        return .{ .Number = @intToFloat(f64, std.time.milliTimestamp()) };
    }
    fn deinit(self: *Self) void {
        self.alloc.destroy(self);
    }

    pub fn callable(alloc: std.mem.Allocator) !Callable {
        var self: Self = .{
            .alloc = alloc,
        };
        var heap = try alloc.create(Self);
        heap.* = self;
        return Callable.new(heap);
    }
};

const UserFn = struct {
    const Self = @This();
    alloc: std.mem.Allocator,
    params: [][]const u8,
    body: *Stmt,

    closure: *Environment,

    fn arity(self: *Self) u8 {
        return @intCast(u8, self.params.len);
    }
    fn call(self: *Self, interpreter: *Interpreter, args: []Value) anyerror!Value {
        var prev_env = interpreter.environment;
        interpreter.environment = try interpreter.enclosed_env(self.closure);
        defer {
            // interpreter.environment.deinit();
            interpreter.environment = prev_env;
        }

        for (self.params) |param, i| {
            try interpreter.environment.define(param, args[i]);
        }

        var r = try interpreter.evaluate_stmt(self.body);
        switch (r) {
            .Void => {},
            .Break => return error.BadBreak,
            .Continue => return error.BadContinue,
            .Return => |val| return val,
        }
        return .None;
    }
    fn deinit(self: *Self) void {
        self.alloc.destroy(self);
    }
};

pub const Interpreter = struct {
    const Self = @This();
    const EnvironmentList = std.ArrayList(*Environment);
    const DeallocatableList = std.ArrayList(Deallocatable);

    alloc: std.mem.Allocator,
    global_env: *Environment,
    environment: *Environment,

    dealloc_list: DeallocatableList,

    fn enclosed_env(self: *Self, env: *Environment) !*Environment {
        var e = try env.enclosed();
        try self.dealloc_list.append(Deallocatable.new(e));
        return e;
    }

    pub fn new(alloc: std.mem.Allocator) !Self {
        var deallocs = DeallocatableList.init(alloc);

        var globals = try alloc.create(Environment);
        globals.* = Environment.new(alloc);
        try deallocs.append(Deallocatable.new(globals));

        var heap_clock = try alloc.create(Clock);
        heap_clock.* = Clock{ .alloc = alloc };
        var clock = Callable.new(heap_clock);
        try deallocs.append(Deallocatable.new(heap_clock));

        try globals.define(
            "clock",
            .{ .Callable = clock },
        );

        return .{
            .alloc = alloc,
            .environment = globals,
            .global_env = globals,
            .dealloc_list = deallocs,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.dealloc_list.items) |*de| {
            de.deinit();
        }
        self.dealloc_list.deinit();
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
            .Call => |val| {
                var callee = try self.eval_expr(val.callee);
                var args = try self.alloc.alloc(Value, val.args.len);
                defer self.alloc.free(args);

                for (val.args) |arg, i| {
                    var v = try self.eval_expr(arg);
                    args[i] = v;
                }

                return try self.call(callee, args);
            },
        }
    }

    fn call(self: *Self, callee: Value, args: []Value) !Value {
        var callable: ?Callable = null;
        switch (callee) {
            .Callable => |c| {
                callable = c;
            },
            else => {},
        }

        if (callable) |cal| {
            var c: Callable = cal;
            // _ = c;
            var u: u8 = c.arity();
            _ = u;
            if (c.arity() != args.len) {
                return error.IncorrectNumberOfArgs;
            }
            return try c.call(self, args);
        } else {
            return error.NotCallable;
        }
    }

    pub fn evaluate_stmt(self: *Self, stmt: *Stmt) !StmtResult {
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
                    .Callable => {
                        std.debug.print("<native callable>", .{});
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
            .Break => return .Break,
            .Continue => return .Continue,
            .Return => |val| {
                if (val.val) |v| {
                    return .{ .Return = try self.eval_expr(v) };
                } else {
                    return .Void;
                }
            },
            .Block => |stmts| {
                var prev = self.environment;
                self.environment = try self.enclosed_env(prev);
                defer {
                    // self.environment.deinit();
                    self.environment = prev;
                }

                for (stmts) |s| {
                    var r = try self.evaluate_stmt(s);
                    if (!r.is_void()) {
                        return r;
                    }
                }
            },
            .If => |s| {
                var condition = try self.eval_expr(s.condition);
                switch (condition) {
                    .Bool => |b| {
                        var r: StmtResult = .Void;
                        if (b) {
                            r = try self.evaluate_stmt(s.if_block);
                        } else if (s.else_block) |blk| {
                            r = try self.evaluate_stmt(blk);
                        }
                        if (!r.is_void()) {
                            return r;
                        }
                    },
                    else => return error.ExpectedBooleanExpression,
                }
            },
            .While => |val| {
                while (true) {
                    var condition = try self.eval_expr(val.condition);
                    if (condition.as_bool()) |b| {
                        if (b) {
                            var r = try self.evaluate_stmt(val.block);
                            switch (r) {
                                .Break => break,
                                .Continue, .Void => {},
                                .Return => return r,
                            }
                        } else {
                            break;
                        }
                    } else {
                        return error.ExpectedBooleanExpression;
                    }
                }
            },
            .For => |val| {
                var prev = self.environment;
                self.environment = try self.enclosed_env(prev);
                defer {
                    // self.environment.deinit();
                    self.environment = prev;
                }

                if (val.start) |s| {
                    var r = try self.evaluate_stmt(s);
                    switch (r) {
                        .Void => {},
                        .Continue => return error.BadContinue,
                        .Break => return error.BadBreak,
                        .Return => return error.BadReturn,
                    }
                }

                while (true) {
                    if (val.mid) |e| {
                        var mid = try self.eval_expr(e);
                        if (mid.as_bool()) |b| {
                            if (!b) {
                                break;
                            }
                        }
                    }

                    var r = try self.evaluate_stmt(val.block);
                    switch (r) {
                        .Break => break,
                        .Continue, .Void => {},
                        .Return => return r,
                    }

                    if (val.end) |s| {
                        r = try self.evaluate_stmt(s);
                        switch (r) {
                            .Void => {},
                            .Continue => return error.BadContinue,
                            .Break => return error.BadBreak,
                            .Return => return error.BadReturn,
                        }
                    }
                }
            },
            .Function => |func| {
                var f = UserFn{
                    .alloc = self.alloc,
                    .params = func.params,
                    .body = func.body,
                    .closure = try self.enclosed_env(self.environment),
                };
                var heap_fn = try self.alloc.create(UserFn);
                heap_fn.* = f;
                try self.dealloc_list.append(Deallocatable.new(heap_fn));
                var function = .{ .Callable = Callable.new(heap_fn) };
                try self.environment.define(func.name, function);
            },
        }
        return .Void;
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
            .Call => |val| {
                self.freeall_expr(val.callee);
                for (val.args) |arg| {
                    self.freeall_expr(arg);
                }
                self.alloc.free(val.args);
            },
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
            .Break, .Continue => {},
            .While => |v| {
                self.freeall_expr(v.condition);
                self.freeall_stmt(v.block);
            },
            .For => |val| {
                if (val.start) |s| {
                    self.freeall_stmt(s);
                }
                if (val.mid) |e| {
                    self.freeall_expr(e);
                }
                if (val.end) |s| {
                    self.freeall_stmt(s);
                }

                self.freeall_stmt(val.block);
            },
            .Function => |func| {
                self.alloc.free(func.params);
                self.freeall_stmt(func.body);
            },
            .Return => |val| {
                if (val.val) |v| {
                    self.freeall_expr(v);
                }
            },
        }
    }
};
