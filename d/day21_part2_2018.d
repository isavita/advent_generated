
import std.stdio;
import std.container.array;

void main() {
    auto register5 = 0;
    auto seen = new bool[](16777216);
    auto lastUnique = 0;

    while (true) {
        auto register3 = register5 | 65536;
        register5 = 7586220;

        while (true) {
            auto register1 = register3 & 255;
            register5 = (((register5 + register1) & 16777215) * 65899) & 16777215;

            if (register3 < 256) {
                if (seen[register5]) {
                    writefln("Part Two Answer: %d", lastUnique);
                    return;
                }
                seen[register5] = true;
                lastUnique = register5;
                break;
            } else {
                register3 /= 256;
            }
        }
    }
}
