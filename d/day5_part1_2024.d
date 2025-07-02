
import std.stdio, std.string, std.conv, std.file, std.algorithm, std.array;

bool isCorrectOrder(const int[] update, const bool[int][int] rules) {
    for (size_t i = 0; i < update.length; i++) {
        for (size_t j = i + 1; j < update.length; j++) {
            auto page1 = update[i];
            auto page2 = update[j];
            if (auto p2Rules = page2 in rules) {
                if (page1 in *p2Rules) {
                    return false;
                }
            }
        }
    }
    return true;
}

void main() {
    auto lines = readText("input.txt").strip().split("\n");
    
    bool[int][int] rules;
    int[][] updates;
    
    size_t i = 0;
    while (i < lines.length && lines[i].canFind('|')) {
        auto parts = lines[i].split('|');
        rules[parts[0].to!int][parts[1].to!int] = true;
        i++;
    }

    for (i++; i < lines.length; i++) {
        if (lines[i].empty) continue;
        updates ~= lines[i].split(',').map!(a => a.to!int).array;
    }

    long sumOfMiddlePages = 0;
    foreach (update; updates) {
        if (isCorrectOrder(update, rules)) {
            sumOfMiddlePages += update[update.length / 2];
        }
    }

    writeln(sumOfMiddlePages);
}
