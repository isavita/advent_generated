
import std.stdio;
import std.conv;
import std.string;
import std.array;
import std.algorithm;

void main() {
    auto numbers = File("input.txt")
                   .byLine
                   .map!(a => to!long(a.strip))
                   .array;

    immutable int preamble = 25;

    for (size_t i = preamble; i < numbers.length; ++i) {
        auto target = numbers[i];
        auto window = numbers[i - preamble .. i];

        bool isValid() {
            bool[long] seen;
            foreach (num; window) {
                if ((target - num) in seen) {
                    return true;
                }
                seen[num] = true;
            }
            return false;
        }

        if (!isValid()) {
            writeln(target);
            return;
        }
    }
}
