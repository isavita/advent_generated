import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.string;

struct Claim {
    int id;
    int left;
    int top;
    int width;
    int height;
}

void main() {
    string input = cast(string) read("input.txt");
    auto claims = input.split("\n").map!(parseClaim).array;

    int[1000][1000] fabric;
    foreach (claim; claims) {
        foreach (y; claim.top .. claim.top + claim.height) {
            foreach (x; claim.left .. claim.left + claim.width) {
                fabric[y][x]++;
            }
        }
    }

    foreach (claim; claims) {
        bool intact = true;
        foreach (y; claim.top .. claim.top + claim.height) {
            foreach (x; claim.left .. claim.left + claim.width) {
                if (fabric[y][x] > 1) {
                    intact = false;
                    break;
                }
            }
            if (!intact) break;
        }
        if (intact) {
            writeln(claim.id);
            return;
        }
    }
}

Claim parseClaim(string line) {
    auto parts = line.split(" ");
    int id = parts[0][1..$].to!int;
    auto position = parts[2].stripRight(":").split(",");
    int left = position[0].to!int;
    int top = position[1].to!int;
    auto size = parts[3].split("x");
    int width = size[0].to!int;
    int height = size[1].to!int;

    return Claim(id, left, top, width, height);
}