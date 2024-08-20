import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.format;

struct Position {
    int x, y;
}

void main() {
    auto file = File("input.txt", "r");
    if (!file.isOpen) {
        writeln("Failed to open file.");
        return;
    }

    Position head = Position(0, 0);
    Position tail = Position(0, 0);
    bool[string] visited;
    visited["0,0"] = true;

    foreach (line; file.byLine()) {
        auto parts = line.split(" ");
        if (parts.length != 2) continue;

        char dir = parts[0][0];
        int steps = to!int(parts[1]);

        foreach (_; 0 .. steps) {
            moveHead(dir, head);
            moveTail(head, tail);
            visited["%s,%s".format(tail.x, tail.y)] = true;
        }
    }

    writeln(visited.length);
}

void moveHead(char dir, ref Position head) {
    switch (dir) {
        case 'U': head.y++; break;
        case 'D': head.y--; break;
        case 'L': head.x--; break;
        case 'R': head.x++; break;
        default: break;
    }
}

void moveTail(Position head, ref Position tail) {
    if (abs(head.x - tail.x) > 1 || abs(head.y - tail.y) > 1) {
        if (head.x > tail.x) tail.x++;
        else if (head.x < tail.x) tail.x--;

        if (head.y > tail.y) tail.y++;
        else if (head.y < tail.y) tail.y--;
    }
}

int abs(int x) {
    return x < 0 ? -x : x;
}