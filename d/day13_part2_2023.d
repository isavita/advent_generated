
import std.stdio;
import std.file;
import std.string;
import std.algorithm;
import std.array;

uint findReflection(const uint[] lines) {
    for (size_t i = 1; i < lines.length; i++) {
        int smudges = 0;
        foreach (j; 0 .. min(i, lines.length - i)) {
            uint diff = lines[i - 1 - j] ^ lines[i + j];
            if (diff == 0) continue;
            if ((diff & (diff - 1)) != 0) {
                smudges = 2;
                break;
            }
            smudges++;
        }
        if (smudges == 1) return cast(uint)i;
    }
    return 0;
}

void main() {
    long total = 0;
    foreach (block; readText("input.txt").strip().split("\n\n")) {
        auto lines = block.splitLines();
        if (lines.empty) continue;

        auto height = lines.length;
        auto width = lines[0].length;

        auto rows = new uint[height];
        auto cols = new uint[width];

        foreach (y, line; lines) {
            uint val = 0;
            foreach (c; line) {
                val = (val << 1) | (c == '#');
            }
            rows[y] = val;
        }

        foreach (x; 0 .. width) {
            uint val = 0;
            foreach (y; 0 .. height) {
                val = (val << 1) | (lines[y][x] == '#');
            }
            cols[x] = val;
        }

        total += findReflection(cols);
        total += findReflection(rows) * 100;
    }
    writeln(total);
}
