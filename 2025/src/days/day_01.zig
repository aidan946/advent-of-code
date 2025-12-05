const std = @import("std");

pub fn print() !void {
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
}
