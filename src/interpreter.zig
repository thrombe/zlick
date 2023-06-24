const std = @import("std");

const TokenType = @import("./lexer.zig").TokenType;
const LigErr = @import("./main.zig").LigErr;
const Stmt = @import("./parser.zig").Stmt;
const Expr = @import("./parser.zig").Expr;
const dbg = std.debug.print;

pub const Value = union(enum) {
    String: []const u8,
    Number: f64,
    None,
    Bool: bool,
    Callable: Callable,
    Class: *Class,
    Object: *Object,

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

    self: *anyopaque,
    vtable: *const struct {
        arityFn: TypeArityFn,
        callFn: TypeCallFn,
    },

    pub fn arity(self: *Self) u8 {
        // fn alloc(ctx: *anyopaque, len: usize, log2_ptr_align: u8, ret_addr: usize) ?[*]u8 {
        //     const self = @ptrCast(*Self, @alignCast(@alignOf(Self), ctx));
        return self.vtable.arityFn(self.self);
    }
    pub fn call(self: *Self, interpreter: *Interpreter, args: []Value) anyerror!Value {
        return self.vtable.callFn(self.self, interpreter, args);
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
            },
        };
    }
};

const Deallocatable = struct {
    const Self = @This();
    pub const TypeDeinitFn = *const fn (self: *anyopaque, alloc: std.mem.Allocator) void;

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

    pub fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        self.deinitFn(self.self, alloc);
    }
};

const Clock = struct {
    const Self = @This();

    fn arity(_: *Self) u8 {
        return 0;
    }
    fn call(_: *Self, _: *Interpreter, _: []Value) anyerror!Value {
        return .{ .Number = @intToFloat(f64, std.time.milliTimestamp()) };
    }
    fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        alloc.destroy(self);
    }

    pub fn callable(alloc: std.mem.Allocator) !Callable {
        var self: Self = .{};
        var heap = try alloc.create(Self);
        heap.* = self;
        return Callable.new(heap);
    }
};

const InitialiserFn = struct {
    const Self = @This();

    inner: *UserFn,
    fn arity(self: *Self) u8 {
        return self.inner.arity();
    }
    fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        alloc.destroy(self);
    }
    fn call(self: *Self, interpterer: *Interpreter, args: []Value) anyerror!Value {
        var ret = try self.inner.call(interpterer, args);
        if (ret != .None) {
            return error.BadInitialiserReturn;
        } else {
            return try self.inner.closure.get("self");
        }
    }
};
const UserFn = struct {
    const Self = @This();
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
    fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        alloc.destroy(self);
    }
};

const Class = struct {
    const Self = @This();
    const Methods = std.StringHashMap(*UserFn);

    name: []const u8,
    methods: Methods,

    fn arity(self: *Self) u8 {
        if (self.methods.get("init")) |init| {
            return init.arity();
        } else {
            return 0;
        }
    }
    fn call(self: *Self, interpreter: *Interpreter, args: []Value) anyerror!Value {
        var ob = try interpreter.alloc.create(Object);
        ob.* = Object.new(self, interpreter.alloc);
        try interpreter.dealloc_list.append(Deallocatable.new(ob));
        var object = .{ .Object = ob };

        if (self.methods.get("init")) |init| {
            var func = try ob.bound_fn(interpreter, init);
            var ret = try func.call(interpreter, args);
            if (ret != .None) {
                return error.BadInitialiserReturn;
            }
        }

        return object;
    }

    fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        self.methods.deinit();
        alloc.destroy(self);
    }

    fn get_method(self: *Self, name: []const u8) ?*UserFn {
        return self.methods.get(name);
    }
};

const Object = struct {
    const Self = @This();
    const Fields = std.StringHashMap(Value);

    class: *Class,
    fields: Fields,

    fn new(class: *Class, alloc: std.mem.Allocator) Self {
        return .{
            .class = class,
            .fields = Fields.init(alloc),
        };
    }

    fn deinit(self: *Self, alloc: std.mem.Allocator) void {
        self.fields.deinit();
        alloc.destroy(self);
    }

    pub fn get(self: *Self, name: []const u8, interpreter: *Interpreter) !Value {
        if (self.fields.get(name)) |val| {
            return val;
        } else if (self.class.get_method(name)) |val| {
            var bound = try self.bound_fn(interpreter, val);
            if (std.mem.eql(u8, name, "init")) {
                var init = try interpreter.alloc.create(InitialiserFn);
                init.* = InitialiserFn{ .inner = bound };
                try interpreter.dealloc_list.append(Deallocatable.new(init));
                return .{ .Callable = Callable.new(init) };
            } else {
                return .{ .Callable = Callable.new(bound) };
            }
        } else {
            return error.UndefinedProperty;
        }
    }

    pub fn set(self: *Self, name: []const u8, val: Value) !void {
        try self.fields.put(name, val);
    }

    pub fn bound_fn(self: *Self, interpreter: *Interpreter, function: *UserFn) !*UserFn {
        var func = try interpreter.alloc.create(UserFn);
        try interpreter.dealloc_list.append(Deallocatable.new(func));

        func.* = function.*;

        func.closure = try interpreter.enclosed_env(function.closure);
        try func.closure.define("self", .{ .Object = self });

        return func;
    }
};

pub const Interpreter = struct {
    const Self = @This();
    const EnvironmentList = std.ArrayList(*Environment);
    const DeallocatableList = std.ArrayList(Deallocatable);
    const ExprDepthMap = std.AutoHashMap(*Expr, usize);

    alloc: std.mem.Allocator,
    // TODO: variables can be stored in a flat array as there is already a map (ExprDepthMap)
    // that stores some information about variables. that could also store information
    // about index of that variable in the array. (map has one entry associated with
    // each usage of a variable (except global variables for some reason.))
    global_env: *Environment,
    environment: *Environment,
    locals: ExprDepthMap,

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
        heap_clock.* = Clock{};
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
            .locals = ExprDepthMap.init(alloc),
        };
    }

    fn resolve(self: *Self, expr: *Expr, depth: usize) !void {
        try self.locals.put(expr, depth);
    }

    fn assign(self: *Self, name: []const u8, val: Value, expr: *Expr) !void {
        if (self.locals.get(expr)) |dist| {
            var d = dist;
            var env = self.environment;
            while (env.super) |s| {
                if (d == 0) {
                    break;
                }
                env = s;
                d -= 1;
            }
            if (d != 0) {
                return error.BadDepth;
            }
            try env.set(name, val);
        } else {
            try self.global_env.set(name, val);
        }
    }

    fn lookup_var(self: *Self, name: []const u8, expr: *Expr) !Value {
        if (self.locals.get(expr)) |dist| {
            var d = dist;
            var env = self.environment;
            while (env.super) |s| {
                if (d == 0) {
                    break;
                }
                env = s;
                d -= 1;
            }
            if (d != 0) {
                return error.BadDepth;
            }
            return try env.get(name);
        } else {
            return try self.global_env.get(name);
        }
    }

    pub fn deinit(self: *Self) void {
        for (self.dealloc_list.items) |*de| {
            de.deinit(self.alloc);
        }
        self.dealloc_list.deinit();
        self.locals.deinit();
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
                return try self.lookup_var(name, expr);
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
            .Get => |val| {
                var obj = try self.eval_expr(val.object);
                switch (obj) {
                    .Object => |o| {
                        return try o.get(val.name, self);
                    },
                    else => return error.NotObject,
                }
            },
            .Self => {
                return try self.lookup_var("self", expr);
            },
        }
    }

    fn call(self: *Self, callee: Value, args: []Value) !Value {
        var callable: ?Callable = null;
        switch (callee) {
            .Callable => |c| {
                callable = c;
            },
            .Class => |class| {
                callable = Callable.new(class);
            },
            else => {},
        }

        if (callable) |*c| {
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
                    .Class => |class| {
                        std.debug.print("<class {s}>", .{class.name});
                    },
                    .Object => |ob| {
                        std.debug.print("<object of class {s}>", .{ob.class.name});
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
                try self.assign(val.name, value, val.expr);
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
                    .params = func.params,
                    .body = func.body,
                    .closure = self.environment,
                };
                var heap_fn = try self.alloc.create(UserFn);
                heap_fn.* = f;
                try self.dealloc_list.append(Deallocatable.new(heap_fn));
                var function = .{ .Callable = Callable.new(heap_fn) };
                try self.environment.define(func.name, function);
            },
            .Class => |class| {
                var methods = Class.Methods.init(self.alloc);

                for (class.methods) |method| {
                    var f = UserFn{
                        .params = method.params,
                        .body = method.body,
                        .closure = self.environment,
                    };
                    var heap_fn = try self.alloc.create(UserFn);
                    heap_fn.* = f;
                    try self.dealloc_list.append(Deallocatable.new(heap_fn));

                    try methods.put(method.name, heap_fn);
                }

                var c = try self.alloc.create(Class);
                c.* = .{ .name = class.name, .methods = methods };
                try self.dealloc_list.append(Deallocatable.new(c));
                try self.environment.define(class.name, .{ .Class = c });
            },
            .Set => |val| {
                var obj = try self.eval_expr(val.object);
                switch (obj) {
                    .Object => |o| {
                        var value = try self.eval_expr(val.value);
                        try o.set(val.name, value);
                    },
                    else => return error.NotObject,
                }
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
            .Get => |val| {
                self.freeall_expr(val.object);
            },
            .Self => {},
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
            .Class => |val| {
                for (val.methods) |method| {
                    self.alloc.free(method.params);
                    self.freeall_stmt(method.body);
                }
                self.alloc.free(val.methods);
            },
            .Set => |val| {
                self.freeall_expr(val.value);
                self.freeall_expr(val.object);
            },
        }
    }
};

pub const ScopeResolver = struct {
    const Self = @This();
    const Scope = std.StringHashMap(bool);
    const ScopeList = std.ArrayList(Scope);

    const FuncType = enum {
        None,
        Function,
    };
    const ControlFlow = enum {
        While,
        For,
        None,
    };
    const ClassType = enum {
        None,
        Class,
    };

    in_func: FuncType,
    in_loop: ControlFlow,
    in_class: ClassType,

    alloc: std.mem.Allocator,
    scopes: ScopeList,
    interpreter: *Interpreter,

    pub fn new(alloc: std.mem.Allocator, interpreter: *Interpreter) Self {
        return .{
            .alloc = alloc,
            .interpreter = interpreter,
            .scopes = ScopeList.init(alloc),
            .in_func = .None,
            .in_loop = .None,
            .in_class = .None,
        };
    }

    pub fn deinit(self: *Self) void {
        self.scopes.deinit();
    }

    fn begin_scope(self: *Self) !void {
        try self.scopes.append(Scope.init(self.alloc));
    }

    fn end_scope(self: *Self) void {
        var scope = self.scopes.pop();
        scope.deinit();
    }

    fn peek(self: *Self) *Scope {
        return &self.scopes.items[self.scopes.items.len - 1];
    }

    fn declare(self: *Self, name: []const u8) !void {
        if (self.scopes.items.len == 0) {
            return;
        }

        if (self.peek().contains(name)) {
            return error.BadLetBinding;
        }
        try self.peek().put(name, false);
    }

    fn define(self: *Self, name: []const u8) !void {
        if (self.scopes.items.len == 0) {
            return;
        }
        try self.peek().put(name, true);
    }

    pub fn resolve_expr(self: *Self, expr: *Expr) !void {
        switch (expr.*) {
            .Variable => |name| {
                if (self.scopes.items.len > 0) {
                    if (self.peek().get(name)) |v| {
                        if (!v) {
                            return error.BadVarInitialiser;
                        }
                    }
                }

                for (self.scopes.items) |_, i| {
                    var j = self.scopes.items.len - i - 1;
                    if (self.scopes.items[j].contains(name)) {
                        try self.interpreter.resolve(expr, i);
                        break;
                    }
                }
            },
            .Self => {
                switch (self.in_class) {
                    .Class => switch (self.in_func) {
                        .Function => {},
                        .None => return error.BadSelf,
                    },
                    .None => return error.BadSelf,
                }

                for (self.scopes.items) |_, i| {
                    var j = self.scopes.items.len - i - 1;
                    if (self.scopes.items[j].contains("self")) {
                        try self.interpreter.resolve(expr, i);
                        break;
                    }
                }
            },
            .Binary => |val| {
                try self.resolve_expr(val.left);
                try self.resolve_expr(val.right);
            },
            .Unary => |val| {
                try self.resolve_expr(val.oparand);
            },
            .Literal => {},
            .Group => |val| {
                try self.resolve_expr(val);
            },
            .Call => |val| {
                try self.resolve_expr(val.callee);
                for (val.args) |arg| {
                    try self.resolve_expr(arg);
                }
            },
            .Get => |val| {
                try self.resolve_expr(val.object);
            },
        }
    }

    fn resolve_func(self: *Self, func: Stmt.Function) !void {
        try self.begin_scope();
        defer self.end_scope();

        for (func.params) |param| {
            try self.define(param);
        }
        try self.resolve_stmt(func.body);
    }

    pub fn resolve_stmt(self: *Self, stmt: *Stmt) anyerror!void {
        switch (stmt.*) {
            .Block => |stmts| {
                try self.begin_scope();
                defer self.end_scope();

                for (stmts) |s| {
                    try self.resolve_stmt(s);
                }
            },
            .Function => |func| {
                var in_loop = self.in_loop;
                self.in_loop = .None;
                defer self.in_loop = in_loop;
                var in_func = self.in_func;
                self.in_func = .Function;
                defer self.in_func = in_func;

                try self.define(func.name);

                try self.resolve_func(func);
            },
            .Let => |e| {
                try self.declare(e.name);
                if (e.init_expr) |ne| {
                    try self.resolve_expr(ne);
                }
                try self.define(e.name);
            },
            .Assign => |v| {
                try self.resolve_expr(v.expr);

                for (self.scopes.items) |_, i| {
                    var j = self.scopes.items.len - i - 1;
                    if (self.scopes.items[j].contains(v.name)) {
                        try self.interpreter.resolve(v.expr, i);
                        break;
                    }
                }
            },
            .Class => |class| {
                var in_loop = self.in_loop;
                self.in_loop = .None;
                defer self.in_loop = in_loop;
                var in_func = self.in_func;
                self.in_func = .Function;
                defer self.in_func = in_func;
                var in_class = self.in_class;
                self.in_class = .Class;
                defer self.in_class = in_class;

                try self.define(class.name);

                try self.begin_scope();
                defer self.end_scope();

                try self.define("self");
                for (class.methods) |method| {
                    try self.resolve_func(method);
                }
            },
            .Print => |e| {
                try self.resolve_expr(e);
            },
            .Expr => |e| {
                try self.resolve_expr(e);
            },
            .If => |v| {
                try self.resolve_expr(v.condition);
                try self.resolve_stmt(v.if_block);
                if (v.else_block) |b| {
                    try self.resolve_stmt(b);
                }
            },
            .Continue => {
                switch (self.in_loop) {
                    .None => return error.BadContinue,
                    .For, .While => {},
                }
            },
            .Break => {
                switch (self.in_loop) {
                    .None => return error.BadBreak,
                    .For, .While => {},
                }
            },
            .While => |v| {
                var in_loop = self.in_loop;
                self.in_loop = .While;
                defer self.in_loop = in_loop;

                try self.resolve_expr(v.condition);
                try self.resolve_stmt(v.block);
            },
            .For => |val| {
                var in_loop = self.in_loop;
                self.in_loop = .For;
                defer self.in_loop = in_loop;

                try self.begin_scope();
                defer self.end_scope();

                if (val.start) |s| {
                    try self.resolve_stmt(s);
                }
                if (val.mid) |e| {
                    try self.resolve_expr(e);
                }
                if (val.end) |s| {
                    try self.resolve_stmt(s);
                }

                try self.resolve_stmt(val.block);
            },
            .Return => |val| {
                switch (self.in_func) {
                    .None => return error.BadReturn,
                    .Function => {},
                }

                if (val.val) |v| {
                    try self.resolve_expr(v);
                }
            },
            .Set => |val| {
                try self.resolve_expr(val.value);
                try self.resolve_expr(val.object);
            },
        }
    }
};
