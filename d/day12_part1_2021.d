
import std.stdio, std.file, std.string, std.uni;

string[][string] adj;
long count;

void dfs(string current, ref bool[string] visited) {
    if (current == "end") {
        count++;
        return;
    }

    foreach (next; adj.get(current, null)) {
        bool isSmall = isLower(next[0]);
        if (isSmall && next in visited) {
            continue;
        }

        if (isSmall) {
            visited[next] = true;
        }

        dfs(next, visited);

        if (isSmall) {
            visited.remove(next);
        }
    }
}

void main() {
    foreach (line; readText("input.txt").splitLines()) {
        if (line.empty) continue;
        auto parts = line.split('-');
        adj[parts[0]] ~= parts[1];
        adj[parts[1]] ~= parts[0];
    }

    bool[string] visited;
    visited["start"] = true;
    dfs("start", visited);

    writeln(count);
}
