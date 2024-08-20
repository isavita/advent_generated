import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.string;

void main()
{
    string input = cast(string) read("input.txt");
    auto lines = input.split("\n");

    int[][] graph;
    foreach (line; lines)
    {
        auto parts = line.split(" <-> ");
        int id = to!int(parts[0]);
        auto neighbors = parts[1].split(", ").map!(to!int).array;
        if (id >= graph.length)
            graph.length = id + 1;
        graph[id] = neighbors;
    }

    bool[] visited;
    visited.length = graph.length;
    dfs(0, graph, visited);

    int count = 0;
    foreach (v; visited)
    {
        if (v)
            count++;
    }

    writeln(count);
}

void dfs(int node, int[][] graph, bool[] visited)
{
    if (visited[node])
        return;
    visited[node] = true;
    foreach (neighbor; graph[node])
    {
        dfs(neighbor, graph, visited);
    }
}