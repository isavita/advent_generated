
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.range;
import std.array;

void swapPositions(ref char[] pw, size_t x, size_t y) {
    swap(pw[x], pw[y]);
}

void swapLetters(ref char[] pw, char x, char y) {
    auto ix = pw.countUntil(x);
    auto iy = pw.countUntil(y);
    swap(pw[ix], pw[iy]);
}

void rotate(ref char[] pw, int steps) {
    auto len = cast(int)pw.length;
    if (len == 0) return;
    steps %= len;
    if (steps < 0) {
        steps += len;
    }
    if (steps == 0) return;
    pw = pw[$ - steps .. $] ~ pw[0 .. $ - steps];
}

void derotateLetter(ref char[] pw, char x) {
    auto index = pw.countUntil(x);
    int rot;
    if (index % 2 == 1) {
        rot = -(cast(int)index + 1) / 2;
    } else if (index == 0) {
        rot = -1;
    } else {
        rot = (6 - cast(int)index) / 2;
    }
    rotate(pw, rot);
}

void reverseSub(ref char[] pw, size_t x, size_t y) {
    pw[x .. y + 1].reverse();
}

void move(ref char[] pw, size_t from, size_t to) {
    char c = pw[from];
    auto temp = pw[0 .. from] ~ pw[from + 1 .. $];
    pw = temp[0 .. to] ~ c ~ temp[to .. $];
}

void main() {
    auto instructions = readText("input.txt").strip.splitLines;
    char[] password = "fbgdceah".dup;

    foreach (line; instructions.retro) {
        auto parts = line.split;
        if (parts[0] == "swap") {
            if (parts[1] == "position") {
                swapPositions(password, to!size_t(parts[2]), to!size_t(parts[5]));
            } else {
                swapLetters(password, parts[2][0], parts[5][0]);
            }
        } else if (parts[0] == "rotate") {
            if (parts[1] == "based") {
                derotateLetter(password, parts[6][0]);
            } else {
                auto steps = to!int(parts[2]);
                if (parts[1] == "right") {
                    rotate(password, -steps);
                } else {
                    rotate(password, steps);
                }
            }
        } else if (parts[0] == "reverse") {
            reverseSub(password, to!size_t(parts[2]), to!size_t(parts[4]));
        } else if (parts[0] == "move") {
            move(password, to!size_t(parts[5]), to!size_t(parts[2]));
        }
    }
    writeln(password);
}
