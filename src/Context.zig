const std = @import("std");
const lua = @import("lua.zig");
const Mutex = std.Thread.Mutex;
const Self = @This();

const LuaState = lua.lua_State;
const log = std.log.scoped(.context);

mutex: *Mutex,
luaState: ?*LuaState,

pub const LuaResultCode = enum {
    ok,
    yield,
    errRuntime,
    errSyntax,
    errMem,
    errErr,
    errFile,
};

const LuaError = enum(u8) {
    runtime = 2,
    syntax = 3,
    memory = 4,
    errerr = 5,
};

const LuaResult = union(LuaResultCode) {
    ok: []const u8,
    yield: void,
    errRuntime: void,
    errSyntax: []const u8,
    errMem: void,
};

pub fn init(self: *Self, mutex: *Mutex) void {
    self.* = .{
        .mutex = mutex,
        .luaState = lua.luaL_newstate(),
    };

    lua.luaL_openlibs(self.luaState);
}

fn lua_remove(state: ?*lua.lua_State, idx: i32) void {
    lua.lua_rotate(state, idx, -1);
    lua.lua_pop(state, 1);
}

pub fn eval(self: *Self, str: []const u8) LuaResultCode {
    var result = @intToEnum(LuaResultCode, lua.luaL_loadstring(self.luaState, str.ptr));
    if (result == .ok) {
        result = @intToEnum(LuaResultCode, lua.lua_pcallk(self.luaState, 0, lua.LUA_MULTRET, 0, 0, null));
        return result;
    }
    lua.lua_pop(self.luaState, 1);
    return result;
}

/// Check that the lua module 'lib' is available. Calls require,
/// so it might alter globals.
pub fn checkForLuaLib(self: *Self, lib: []const u8) bool {
    _ = lua.lua_getglobal(self.luaState, "require");
    _ = lua.lua_pushstring(self.luaState, lib.ptr);

    // require returns two things: the module and how it found the module
    const result = lua.lua_pcallk(self.luaState, 1, lua.LUA_MULTRET, 0, 0, null);
    if (result == lua.LUA_OK) {
        lua.lua_pop(self.luaState, 2);
        return true;
    }

    log.debug("lua module '{s}' not found, require returned code {}", .{ lib, result });
    log.debug("message: {s}", .{lua.lua_tolstring(self.luaState, -1, null)});

    // ensure a balanced stack
    lua.lua_settop(self.luaState, 0);
    log.debug("lua stack size {}", .{lua.lua_gettop(self.luaState)});
    return false;
}

/// Load and evaluate the Lua file specified by 'path'.
pub fn loadFile(self: *Self, path: [:0]const u8) LuaResultCode {
    const held = self.mutex.acquire();
    defer held.release();

    var result = lua.luaL_loadfilex(self.luaState, path, null);
    if (result != lua.LUA_OK) {
        return @intToEnum(LuaResultCode, result);
    }

    result = lua.lua_pcallk(self.luaState, 0, lua.LUA_MULTRET, 0, 0, null);

    return @intToEnum(LuaResultCode, result);
}

pub fn register(self: *Self, name: []const u8, func: fn (?*LuaState) callconv(.C) c_int) void {
    const held = self.mutex.acquire();
    defer held.release();

    lua.lua_register(self.luaState, @ptrCast([*c]const u8, name), func);
}

pub fn evalString(self: *Self, allocator: *std.mem.Allocator, str: []const u8) !LuaResult {
    const held = self.mutex.acquire();
    defer held.release();

    // read
    var readResult: LuaResultCode = try readReturn(self.luaState, allocator, str);
    if (readResult == .ok) {
        log.debug("return worked, popping 1", .{});
    } else {
        log.debug("return didn't work, popping 1", .{});
        lua.lua_pop(self.luaState, 1); // remove the failed result
        readResult = try read(self.luaState, str);
    }

    log.debug("read result: {}, dumping stack", .{readResult});

    // evaluate
    var evalResult: LuaResultCode = undefined;
    if (readResult == .ok) {
        const call = lua.lua_pcallk(self.luaState, 0, lua.LUA_MULTRET, 0, 0, null);
        evalResult = @intToEnum(LuaResultCode, call);
    } else {
        const msg = lua.lua_tolstring(self.luaState, -1, null);
        const r: LuaResult = .{ .errSyntax = std.mem.span(msg) };
        return r;
    }

    // print
    return switch (evalResult) {
        .ok => .{ .ok = try getResultString(allocator, self.luaState) },
        .errSyntax, .errMem, .errRuntime, .yield => unreachable,
    };
}

pub fn getLuaVersion(self: *Self) f64 {
    const held = self.mutex.acquire();
    defer held.release();

    return lua.lua_version(self.luaState);
}

fn readReturn(state: ?*LuaState, allocator: *std.mem.Allocator, str: []const u8) !LuaResultCode {
    const buf = try std.fmt.allocPrintZ(allocator, "return {s};", .{str});
    defer allocator.free(buf);

    return try read(state, buf);
}

fn read(state: ?*LuaState, str: []const u8) !LuaResultCode {
    log.debug("read: {s}", .{str});
    return @intToEnum(LuaResultCode, lua.luaL_loadbufferx(state, str.ptr, str.len, "repl", null));
}

fn getResultString(allocator: *std.mem.Allocator, state: ?*LuaState) ![]const u8 {
    const n = lua.lua_gettop(state);

    var paps = try std.ArrayList([]const u8).initCapacity(allocator, @intCast(usize, n));
    defer paps.deinit();

    var i: u8 = 1;
    if (n > 0) {
        while (i <= n) : (i += 1) {
            const fromLua = lua.lua_tolstring(state, i, null);
            var blk: []const u8 = undefined;
            if (fromLua) |string| {
                blk = std.mem.span(string);
            } else blk = "nil";
            try paps.append(blk);
            lua.lua_pop(state, 1);
        }
        return try std.mem.concat(allocator, u8, paps.items);
    } else {
        return "";
    }
}
