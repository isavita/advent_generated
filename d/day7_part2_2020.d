
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.regex;

struct BagRule {
    string color;
    int count;
}

long countBags(string color, BagRule[][string] rules, ref long[string] memo) {
    if (auto p = color in memo) {
        return *p;
    }

    long total = 1;
    if (auto p = color in rules) {
        foreach (rule; *p) {
            total += cast(long)rule.count * countBags(rule.color, rules, memo);
        }
    }

    return memo[color] = total;
}

void main() {
    BagRule[][string] rules;
    auto re = ctRegex!(`(\d+) (\w+ \w+)`);

    foreach (line; readText("input.txt").strip().splitLines()) {
        auto parts = line.split(" bags contain ");
        auto container = parts[0];
        auto contents = parts[1];

        if (contents == "no other bags.") {
            continue;
        }

        foreach (m; contents.matchAll(re)) {
            rules[container] ~= BagRule(m[2].idup, m[1].to!int);
        }
    }

    long[string] memo;
    writeln(countBags("shiny gold", rules, memo) - 1);
}
