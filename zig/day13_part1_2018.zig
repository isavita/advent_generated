
const std = @import("std");

const Cart = struct {
    x: usize,
    y: usize,
    dir: u8,
    turn: u8,
};

fn movingDown(track: [][]u8, cart: Cart) Cart {
    var newCart = cart;
    newCart.y += 1;
    switch (track[newCart.y][newCart.x]) {
        '/' => newCart.dir = '<',
        '\\' => newCart.dir = '>',
        '+' => {
            newCart.dir = ">v<"[newCart.turn];
            newCart.turn = (newCart.turn + 1) % 3;
        },
        else => {},
    }
    return newCart;
}

fn movingUp(track: [][]u8, cart: Cart) Cart {
    var newCart = cart;
    newCart.y -= 1;
    switch (track[newCart.y][newCart.x]) {
        '/' => newCart.dir = '>',
        '\\' => newCart.dir = '<',
        '+' => {
            newCart.dir = "<^>"[newCart.turn];
            newCart.turn = (newCart.turn + 1) % 3;
        },
        else => {},
    }
    return newCart;
}

fn movingLeft(track: [][]u8, cart: Cart) Cart {
    var newCart = cart;
    newCart.x -= 1;
    switch (track[newCart.y][newCart.x]) {
        '/' => newCart.dir = 'v',
        '\\' => newCart.dir = '^',
        '+' => {
            newCart.dir = "v<^"[newCart.turn];
            newCart.turn = (newCart.turn + 1) % 3;
        },
        else => {},
    }
    return newCart;
}

fn movingRight(track: [][]u8, cart: Cart) Cart {
    var newCart = cart;
    newCart.x += 1;
    switch (track[newCart.y][newCart.x]) {
        '/' => newCart.dir = '^',
        '\\' => newCart.dir = 'v',
        '+' => {
            newCart.dir = "^>v"[newCart.turn];
            newCart.turn = (newCart.turn + 1) % 3;
        },
        else => {},
    }
    return newCart;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "input.txt", 1024 * 1024);
    defer allocator.free(input);

    var track = std.ArrayList([]u8).init(allocator);
    defer {
        for (track.items) |row| {
            allocator.free(row);
        }
        track.deinit();
    }

    var carts = std.ArrayList(Cart).init(allocator);
    defer carts.deinit();

    var lines = std.mem.split(u8, input, "\n");
    var y: usize = 0;
    while (lines.next()) |line| {
        var row = try allocator.alloc(u8, line.len);
        for (line, 0..) |c, x| {
            switch (c) {
                '>', '<' => {
                    row[x] = '-';
                    try carts.append(.{ .x = x, .y = y, .dir = c, .turn = 0 });
                },
                '^', 'v' => {
                    row[x] = '|';
                    try carts.append(.{ .x = x, .y = y, .dir = c, .turn = 0 });
                },
                else => row[x] = c,
            }
        }
        try track.append(row);
        y += 1;
    }

    var collision = false;
    while (!collision) {
        for (carts.items) |*cart| {
            cart.* = switch (cart.dir) {
                '>' => movingRight(track.items, cart.*),
                '<' => movingLeft(track.items, cart.*),
                '^' => movingUp(track.items, cart.*),
                'v' => movingDown(track.items, cart.*),
                else => unreachable,
            };
        }

        for (carts.items, 0..) |cart, i| {
            for (carts.items[i + 1..]) |other| {
                if (cart.x == other.x and cart.y == other.y) {
                    collision = true;
                    try std.io.getStdOut().writer().print("{d},{d}\n", .{ cart.x, cart.y });
                    return;
                }
            }
        }
    }
}
