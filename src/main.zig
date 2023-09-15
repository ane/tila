const std = @import("std");
const net = std.net;
const os = std.os;
const fs = std.fs;
const lua = @import("lua.zig");

const ReplServer = @import("ReplServer.zig");
const Context = @import("Context.zig");

const log = std.log.scoped(.fenix);

pub const io_mode = .evented;

pub var context: Context = undefined;

const ConfigType = enum { lua, fennel };

const Giggel = enum { foo, bar };

const Gaggel = union(Giggel) {
    foo: u32,
    bar: bool,
};

fn perset(gugge: Gaggel) u32 {
    switch (gugge) {
        .foo => |num| return num * 2,
        .bar => |bap| if (bap) return 2 else return 1,
    }
}

fn getConfigFile(allocator: std.mem.Allocator) !?[:0]const u8 {
    var path: [:0]const u8 = undefined;

    if (os.getenv("XDG_CONFIG_HOME")) |xdg| {
        path = try fs.path.joinZ(allocator, &.{ xdg, "fenix", "init.lua" });
    } else if (os.getenv("HOME")) |home| {
        path = try fs.path.joinZ(allocator, &[_][]const u8{ home, ".config/fenix/init.lua" });
    } else {
        return null;
    }

    os.accessZ(path, os.R_OK) catch |err| {
        std.log.warn("Unable to open config file at {s}: {s}", .{ path, @errorName(err) });
        allocator.free(path);
        return null;
    };

    return path;
}

const Header = struct {
    version: u8 = 1,
    stop_signal: u8 = 10,
    cont_signal: u8 = 12,
    click_events: bool = true,
};

pub fn loop() !void {
    const stdout = std.io.getStdOut().writer();
    const header = Header{};

    try std.json.stringify(header, .{}, stdout);

    try stdout.print("\n[\n", .{});
    while (true) {
        _ = context.eval("print(tila.foobar)");
        std.time.sleep(100_000_000);
    }
}

pub fn pieru() void {
    while (true) {}
}

pub fn main() anyerror!void {
    const allocator = std.heap.c_allocator;

    var lock = std.Thread.Mutex{};

    context.init(&lock);

    const path = try getConfigFile(allocator);

    if (path) |p| {
        std.log.debug("Using config file at {s}", .{p});
        const configResult = context.loadFile(p);
        switch (configResult) {
            .ok => log.info("config loaded successfully", .{}),
            else => {
                const errName = @tagName(configResult);
                log.warn("error in configuration: loading {s} returned code {s}", .{ p, errName });
            },
        }
    }

    _ = try std.Thread.spawn(.{}, pieru, .{});

    _ = context.eval("tila = {}; tila.foobar = 1;");

    if (context.checkForLuaLib("luarocks.loader") and context.checkForLuaLib("serpent") and context.checkForLuaLib("jeejah")) {
        var main_loop = async loop();
        _ = async ReplServer.start(&context);
        _ = await main_loop;
    } else {
        log.info("lua module 'jeejah' not available, please install it via LuaRocks to enable a REPL", .{});
    }
}
