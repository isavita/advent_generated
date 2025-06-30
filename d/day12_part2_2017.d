
import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.range;

void main() {
    int[][int] adj;
    foreach (line; File("input.txt").byLine()) {
        auto parts = line.strip.split(" <-> ");
        auto fromNode = parts[0].to!int;
        foreach (toNodeStr; parts[1].split(", ")) {
            auto toNode = toNodeStr.to!int;
            adj[fromNode] ~= toNode;
            adj[toNode] ~= fromNode;
        }
    }

    bool[int] visited;
    int groups = 0;

    void dfs(int node) {
        visited[node] = true;
        foreach (neighbor; adj[node]) {
            if (neighbor !in visited) {
                dfs(neighbor);
            }
        }
    }

    foreach (node; adj.keys) {
        if (node !in visited) {
            dfs(node);
            groups++;
        }
    }

    writeln(groups);
}
