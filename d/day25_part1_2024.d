
import std.stdio;
import std.file;
import std.string;
import std.algorithm;
import std.range;

alias Shape = int[5];

Shape parseLock(string[] b) {
    Shape h;
    foreach (c; 0 .. 5) {
        foreach (r; 1 .. 7) {
            if (b[r][c] == '#') h[c]++;
            else break;
        }
    }
    return h;
}

Shape parseKey(string[] b) {
    Shape h;
    foreach (c; 0 .. 5) {
        foreach_reverse (r; 0 .. 6) {
            if (b[r][c] == '#') h[c]++;
            else break;
        }
    }
    return h;
}

bool fits(in Shape lock, in Shape key) {
    foreach (i; 0 .. 5) {
        if (lock[i] + key[i] > 5) return false;
    }
    return true;
}

void main() {
    try {
        auto raw = readText("input.txt")
            .splitLines
            .map!(a => a.strip)
            .filter!(a => !a.empty)
            .array;

        if (raw.length % 7 != 0) {
            writeln(0);
            return;
        }

        Shape[] locks;
        Shape[] keys;

        for (size_t i = 0; i + 7 <= raw.length; i += 7) {
            auto block = raw[i .. i + 7];
            if (block.any!(ln => ln.length < 5)) continue;

            if (block[0].all!(c => c == '#')) {
                locks ~= parseLock(block);
            } else {
                keys ~= parseKey(block);
            }
        }

        long count = 0;
        foreach (ref lock; locks) {
            foreach (ref key; keys) {
                if (fits(lock, key)) {
                    count++;
                }
            }
        }
        writeln(count);
    } catch (FileException) {
        writeln(0);
    }
}
