
import std.stdio, std.file, std.string, std.array;

void main() {
    string[][string] contains;
    foreach (line; readText("input.txt").split("\n")) {
        if (line.empty) continue;
        auto parts = line.split(" bags contain ");
        if (parts[1] == "no other bags.") continue;
        
        auto container = parts[0];
        foreach (bag; parts[1].split(", ")) {
            auto words = bag.split(' ');
            auto name = words[1] ~ " " ~ words[2];
            contains[name] ~= container;
        }
    }

    bool[string] seen;
    void dfs(string bag) {
        if (auto p = bag in contains) {
            foreach (outer; *p) {
                if (outer !in seen) {
                    seen[outer] = true;
                    dfs(outer);
                }
            }
        }
    }

    dfs("shiny gold");
    writeln(seen.length);
}
