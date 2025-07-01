
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.array;

void main() {
    auto lines = readText("input.txt").splitLines();
    int[2][] allRanges;
    long errorRate = 0;
    int section = 0;

    foreach (line; lines) {
        if (line.empty) {
            section++;
            continue;
        }
        if (line.endsWith(":")) {
            continue;
        }

        final switch (section) {
            case 0:
                auto parts = line.split(": ")[1].split(" or ");
                foreach (part; parts) {
                    auto bounds = part.split('-').map!(to!int).array;
                    allRanges ~= [bounds[0], bounds[1]];
                }
                break;
            case 1:
                break;
            case 2:
                foreach (valueStr; line.split(',')) {
                    auto value = valueStr.to!int;
                    if (!any!(r => value >= r[0] && value <= r[1])(allRanges)) {
                        errorRate += value;
                    }
                }
                break;
        }
    }

    writeln(errorRate);
}
