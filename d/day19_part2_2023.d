
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;

struct Rule {
    char category;
    char op;
    int value;
    string dest;
}

struct Interval {
    long start, end;
    long size() const {
        return end >= start ? end - start + 1 : 0;
    }
}

struct PartInterval {
    Interval x, m, a, s;

    ref Interval opIndex(char cat) {
        final switch (cat) {
            case 'x': return x;
            case 'm': return m;
            case 'a': return a;
            case 's': return s;
        }
    }

    long combinations() const {
        return x.size * m.size * a.size * s.size;
    }
}

long countAccepted(string workflowName, PartInterval pInterval, ref Rule[][string] workflows) {
    if (workflowName == "R") return 0;
    if (workflowName == "A") return pInterval.combinations();

    long total = 0;
    auto currentInterval = pInterval;
    auto rules = workflows[workflowName];

    foreach (rule; rules) {
        if (rule.op == '\0') {
            total += countAccepted(rule.dest, currentInterval, workflows);
            continue;
        }

        auto catInterval = &currentInterval.opIndex(rule.category);
        Interval validInterval, invalidInterval;

        if (rule.op == '<') {
            validInterval = Interval(catInterval.start, rule.value - 1);
            invalidInterval = Interval(rule.value, catInterval.end);
        } else {
            validInterval = Interval(rule.value + 1, catInterval.end);
            invalidInterval = Interval(catInterval.start, rule.value);
        }

        if (validInterval.size > 0) {
            auto nextInterval = currentInterval;
            nextInterval.opIndex(rule.category) = validInterval;
            total += countAccepted(rule.dest, nextInterval, workflows);
        }

        *catInterval = invalidInterval;
        if (catInterval.size <= 0) break;
    }
    return total;
}

void main() {
    auto blocks = readText("input.txt").strip.split("\n\n");
    Rule[][string] workflows;

    foreach (line; blocks[0].split("\n")) {
        auto nameAndRules = line.split("{");
        auto name = nameAndRules[0];
        auto rulesStr = nameAndRules[1][0 .. $-1];

        Rule[] rules;
        foreach (ruleStr; rulesStr.split(",")) {
            auto parts = ruleStr.split(":");
            if (parts.length == 1) {
                rules ~= Rule('\0', '\0', 0, parts[0]);
            } else {
                char cat = parts[0][0];
                char op = parts[0][1];
                int val = to!int(parts[0][2..$]);
                string dest = parts[1];
                rules ~= Rule(cat, op, val, dest);
            }
        }
        workflows[name] = rules;
    }

    auto initialInterval = PartInterval(
        Interval(1, 4000),
        Interval(1, 4000),
        Interval(1, 4000),
        Interval(1, 4000)
    );

    auto result = countAccepted("in", initialInterval, workflows);
    writeln(result);
}
