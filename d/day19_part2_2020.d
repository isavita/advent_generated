
import std.stdio, std.file, std.string, std.conv, std.algorithm, std.array, std.typecons;

struct Rule {
    string[] resolved;
    int[][] options;
}

string[][int] memo;

string[] fillInGraph(ref Rule[int] graph, int entry) {
    if (entry in memo) return memo[entry];
    if (graph[entry].resolved.length > 0) {
        return memo[entry] = graph[entry].resolved;
    }

    string[] allResolved;
    foreach (option; graph[entry].options) {
        string[] resolvedForOption = [""];
        foreach (entryPoint; option) {
            auto nestedResolveVals = fillInGraph(graph, entryPoint);
            string[] newResolved;
            foreach (prev; resolvedForOption) {
                foreach (nextPiece; nestedResolveVals) {
                    newResolved ~= prev ~ nextPiece;
                }
            }
            resolvedForOption = newResolved;
        }
        allResolved ~= resolvedForOption;
    }
    return memo[entry] = graph[entry].resolved = allResolved;
}

auto parseInput(string input) {
    auto parts = input.split("\n\n");
    Rule[int] rules;
    
    foreach (line; parts[0].split("\n")) {
        auto ruleParts = line.split(": ");
        auto key = ruleParts[0].to!int;
        
        if (ruleParts[1][0] == '"') {
            rules[key] = Rule([ruleParts[1][1 .. $-1]], []);
        } else {
            auto options = ruleParts[1].split(" | ")
                                       .map!(opt => opt.split(" ").map!(to!int).array)
                                       .array;
            rules[key] = Rule([], options);
        }
    }

    auto messages = parts[1].split("\n").filter!(a => !a.empty).array;
    return tuple(rules, messages);
}

int solve(string input) {
    auto parsed = parseInput(input);
    auto rules = parsed[0];
    auto messages = parsed[1];
    memo.clear();

    auto res42 = fillInGraph(rules, 42);
    auto res31 = fillInGraph(rules, 31);

    bool[string] set42, set31;
    foreach(s; res42) set42[s] = true;
    foreach(s; res31) set31[s] = true;

    auto chunkLen = res42[0].length;
    int validCount = 0;

    foreach (message; messages) {
        if (message.length % chunkLen != 0) continue;
        auto numChunks = message.length / chunkLen;
        if (numChunks < 3) continue;

        int count31 = 0;
        while (count31 < numChunks && (message[$ - (count31 + 1) * chunkLen .. $ - count31 * chunkLen] in set31)) {
            count31++;
        }
        if (count31 == 0) continue;

        int count42 = 0;
        while (count42 < numChunks - count31 && (message[count42 * chunkLen .. (count42 + 1) * chunkLen] in set42)) {
            count42++;
        }

        if (count42 + count31 == numChunks && count42 > count31) {
            validCount++;
        }
    }
    return validCount;
}

void main() {
    writeln(solve(readText("input.txt").strip));
}
