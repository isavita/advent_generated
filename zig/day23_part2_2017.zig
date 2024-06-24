
const std = @import("std");

fn isPrime(n: usize) bool {
    if (n < 2) return false;
    if (n == 2) return true;
    if (n % 2 == 0) return false;
    var i: usize = 3;
    while (i * i <= n) : (i += 2) {
        if (n % i == 0) return false;
    }
    return true;
}

pub fn main() !void {
    const b: usize = 57 * 100 + 100000;
    const c: usize = b + 17000;
    var h: usize = 0;

    var x: usize = b;
    while (x <= c) : (x += 17) {
        if (!isPrime(x)) h += 1;
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{h});
}
