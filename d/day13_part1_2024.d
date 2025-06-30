
import std.stdio, std.string, std.conv, std.algorithm, std.file;

struct Machine {
    long ax, ay, bx, by, px, py;
}

long parseVal(string s) {
    auto idx = s.lastIndexOfAny("=+");
    return s[idx + 1 .. $].strip.to!long;
}

Machine parseMachine(string block) {
    Machine m;
    foreach (line; block.strip.split("\n")) {
        auto parts = line.split(':');
        auto key = parts[0];
        auto values = parts[1].split(',');

        if (key.startsWith("Button A")) {
            m.ax = parseVal(values[0]);
            m.ay = parseVal(values[1]);
        } else if (key.startsWith("Button B")) {
            m.bx = parseVal(values[0]);
            m.by = parseVal(values[1]);
        } else if (key.startsWith("Prize")) {
            m.px = parseVal(values[0]);
            m.py = parseVal(values[1]);
        }
    }
    return m;
}

long solveMachine(Machine m) {
    long minCost = -1;
    foreach (a; 0 .. 101) {
        foreach (b; 0 .. 101) {
            if (m.ax * a + m.bx * b == m.px &&
                m.ay * a + m.by * b == m.py) {
                long cost = a * 3 + b;
                if (minCost == -1 || cost < minCost) {
                    minCost = cost;
                }
            }
        }
    }
    return minCost;
}

void main() {
    long solvedCount = 0;
    long totalCost = 0;

    foreach (block; readText("input.txt").split("\n\n")) {
        if (block.strip.empty) {
            continue;
        }
        auto m = parseMachine(block);
        auto cost = solveMachine(m);
        if (cost != -1) {
            solvedCount++;
            totalCost += cost;
        }
    }

    writefln("%d %d", solvedCount, totalCost);
}
