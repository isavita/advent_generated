
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.array;

void main() {
    long[string] dirSizes;
    string[] path;

    foreach (line; readText("input.txt").splitLines()) {
        auto parts = line.split();
        if (parts[0] == "$") {
            if (parts[1] == "cd") {
                auto dir = parts[2];
                if (dir == "/") {
                    path.length = 0;
                } else if (dir == "..") {
                    path.popBack();
                } else {
                    path ~= dir;
                }
            }
        } else if (parts[0] != "dir") {
            auto size = parts[0].to!long;
            for (int i = 0; i <= path.length; ++i) {
                string key = "/" ~ path[0 .. i].join("/");
                dirSizes[key] += size;
            }
        }
    }

    auto sum = dirSizes.values
                       .filter!(size => size <= 100_000)
                       .sum();
    writeln(sum);
}
