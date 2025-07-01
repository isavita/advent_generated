
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.range;

void main() {
    struct Component {
        int a, b;
    }

    auto components = readText("input.txt")
        .splitLines
        .filter!(a => !a.empty)
        .map!(line => line.split('/'))
        .map!(parts => Component(parts[0].to!int, parts[1].to!int))
        .array;

    int maxStrength = 0;
    int maxLength = 0;

    void findBridge(bool[] used, int port, int strength, int length) {
        if (length > maxLength || (length == maxLength && strength > maxStrength)) {
            maxLength = length;
            maxStrength = strength;
        }

        foreach (i, ref c; components) {
            if (used[i]) continue;

            if (c.a == port || c.b == port) {
                used[i] = true;
                int nextPort = (c.a == port) ? c.b : c.a;
                findBridge(used, nextPort, strength + c.a + c.b, length + 1);
                used[i] = false;
            }
        }
    }

    auto used = new bool[components.length];
    findBridge(used, 0, 0, 0);

    writeln(maxStrength);
}
