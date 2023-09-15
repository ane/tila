const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const exe = b.addExecutable("tila", "src/main.zig");
    exe.addCSourceFile("src/dumpstack.c", &[_][]const u8{"-std=c99"});
    exe.linkLibC();
    exe.addIncludeDir("vendor/lua");
    exe.setTarget(target);
    exe.setBuildMode(mode);

    const lua_c_files = [_][]const u8{
        "lapi.c",
        "lauxlib.c",
        "lbaselib.c",
        "lcode.c",
        "lcorolib.c",
        "lctype.c",
        "ldblib.c",
        "ldebug.c",
        "ldo.c",
        "ldump.c",
        "lfunc.c",
        "lgc.c",
        "linit.c",
        "liolib.c",
        "llex.c",
        "lmathlib.c",
        "lmem.c",
        "loadlib.c",
        "lobject.c",
        "lopcodes.c",
        "loslib.c",
        "lparser.c",
        "lstate.c",
        "lstring.c",
        "lstrlib.c",
        "ltable.c",
        "ltablib.c",
        "ltm.c",
        "lundump.c",
        "lutf8lib.c",
        "lvm.c",
        "lzio.c",
    };

    var cFlags = blk: {
        if (b.release_mode.? == .Debug) {
            break :blk [_][]const u8{
                "-std=c99",
                "-DLUA_USE_DLOPEN",
                "-g",
            };
        } else {
            break :blk [_][]const u8{
                "-DLUA_USE_DLOPEN",
                "-std=c99",
                "-O2",
            };
        }
    };

    inline for (lua_c_files) |c_file| {
        exe.addCSourceFile("vendor/lua/" ++ c_file, &cFlags);
    }

    exe.install();

    const run_cmd = exe.run();
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
