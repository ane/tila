const std = @import("std");
const net = std.net;
const lua = @import("lua.zig");

const Context = @import("Context.zig");
const Self = @This();

const log = std.log.scoped(.server);

pub fn start(
    context: *Context,
) !void {
    log.debug("loading jeejah", .{});
    const result = context.eval("jeejah = require('jeejah'); return jeejah.start(port, {debug=true});");
    if (result == .ok) {
        const luaState = context.luaState;

        const thread: ?*lua.lua_State = lua.lua_tothread(luaState, -1);
        if (thread) |t| {
            log.debug("got thread, starting loop", .{});
            var results: i32 = undefined;
            var yield_res = lua.lua_resume(t, null, 0, &results);

            while (yield_res == lua.LUA_YIELD) {
                yield_res = lua.lua_resume(t, null, 0, &results);
            }
        }
    }
}
