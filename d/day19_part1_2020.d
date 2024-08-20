import std.stdio;
import std.file;
import std.conv;
import std.string;
import std.array;
import std.algorithm;
import std.typecons;

struct Rule {
    string charMatch;
    string[][] subRules;
}

void main() {
    auto input = readText("input.txt").split("\n\n");
    auto rules = parseRules(input[0].split("\n"));
    auto messages = input[1].split("\n");

    int validCount = 0;
    foreach (message; messages) {
        if (matchesRule(message, rules, "0").canFind(message.length)) {
            validCount++;
        }
    }

    writeln(validCount);
}

Rule[string] parseRules(string[] lines) {
    Rule[string] rules;
    foreach (line; lines) {
        auto parts = line.split(": ");
        string ruleNum = parts[0];
        if (parts[1][0] == '"') {
            rules[ruleNum] = Rule(parts[1][1..$-1], []);
        } else {
            auto subRulesParts = parts[1].split(" | ");
            string[][] subRules;
            foreach (subRule; subRulesParts) {
                subRules ~= subRule.split(" ");
            }
            rules[ruleNum] = Rule("", subRules);
        }
    }
    return rules;
}

int[] matchesRule(string message, Rule[string] rules, string ruleNum) {
    auto rule = rules[ruleNum];
    if (rule.charMatch.length > 0) {
        if (message.length > 0 && message[0] == rule.charMatch[0]) {
            return [1];
        }
        return [];
    }

    int[] matches;
    foreach (subRule; rule.subRules) {
        auto lengths = [0];
        foreach (subRuleNum; subRule) {
            int[] newLengths;
            foreach (length; lengths) {
                auto subMatches = matchesRule(message[length..$], rules, subRuleNum);
                foreach (subMatch; subMatches) {
                    newLengths ~= length + subMatch;
                }
            }
            lengths = newLengths;
        }
        matches ~= lengths;
    }
    return matches;
}