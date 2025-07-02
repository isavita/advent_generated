
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.regex;
import std.math;
import std.algorithm;

struct Point {
    long x, y;
}

struct Sensor {
    Point pos;
    long dist;
}

struct Interval {
    long start, end;
}

long manhattan(Point p, Point q) {
    return abs(p.x - q.x) + abs(p.y - q.y);
}

long solve(const Sensor[] sensors, long maxcoord) {
    for (long y = 0; y <= maxcoord; y++) {
        Interval[] intervals;
        foreach (ref s; sensors) {
            long dy = abs(s.pos.y - y);
            if (dy <= s.dist) {
                long remainingDist = s.dist - dy;
                intervals ~= Interval(s.pos.x - remainingDist, s.pos.x + remainingDist);
            }
        }

        if (intervals.empty) {
            return y;
        }

        sort!((a, b) => a.start < b.start)(intervals);

        long x = 0;
        foreach (ref interval; intervals) {
            if (x < interval.start) {
                return x * 4_000_000 + y;
            }
            x = max(x, interval.end + 1);
        }

        if (x <= maxcoord) {
            return x * 4_000_000 + y;
        }
    }
    return -1;
}

void main() {
    Sensor[] sensors;
    auto pattern = ctRegex!r"x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)";

    foreach (line; readText("input.txt").strip.splitLines) {
        auto m = matchFirst(line, pattern);
        Point pos = { m[1].to!long, m[2].to!long };
        Point beacon = { m[3].to!long, m[4].to!long };
        sensors ~= Sensor(pos, manhattan(pos, beacon));
    }

    writeln(solve(sensors, 4_000_000));
}
