import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.string;

struct Node {
    int x;
    int y;
    int used;
    int avail;
}

void main() {
    auto file = File("input.txt", "r");
    auto lines = file.byLineCopy.array;

    auto nodes = lines[2..$].map!(parseNode).array;

    int viablePairs = 0;
    foreach (i, nodeA; nodes) {
        foreach (j, nodeB; nodes) {
            if (i != j && nodeA.used != 0 && nodeA.used <= nodeB.avail) {
                viablePairs++;
            }
        }
    }

    writeln(viablePairs);
}

Node parseNode(string line) {
    auto parts = line.split;
    auto pos = parts[0].split("-");

    auto x = pos[1][1..$].to!int;
    auto y = pos[2][1..$].to!int;
    auto used = parts[2][0..$-1].to!int;
    auto avail = parts[3][0..$-1].to!int;

    return Node(x, y, used, avail);
}