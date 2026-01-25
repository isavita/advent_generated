
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Program
{
    private static Dictionary<string, int> vertexMap = new Dictionary<string, int>();
    private static int vertexCount = 0;

    private class Edge
    {
        public int U { get; set; }
        public int V { get; set; }
        public bool Active { get; set; }
        public int Id { get; set; }
    }

    private class AdjNode
    {
        public int NeighborId { get; set; }
        public int EdgeId { get; set; }
    }

    private static List<Edge> edges = new List<Edge>();
    private static List<List<AdjNode>> adj = new List<List<AdjNode>>();

    private static int GetVertexId(string name)
    {
        if (!vertexMap.TryGetValue(name, out int id))
        {
            id = vertexCount++;
            vertexMap[name] = id;
            adj.Add(new List<AdjNode>());
        }
        return id;
    }

    private static void AddEdge(int u, int v)
    {
        int edgeId = edges.Count;
        edges.Add(new Edge { U = u, V = v, Active = true, Id = edgeId });
        adj[u].Add(new AdjNode { NeighborId = v, EdgeId = edgeId });
        adj[v].Add(new AdjNode { NeighborId = u, EdgeId = edgeId });
    }

    private class BfsResult
    {
        public bool Found { get; set; }
        public int[] ParentEdge { get; set; }
    }

    private static BfsResult BreadthFirstSearch(int startNode, int endNode)
    {
        var result = new BfsResult { Found = false, ParentEdge = new int[vertexCount] };
        for (int i = 0; i < vertexCount; i++) result.ParentEdge[i] = -1;

        var visited = new bool[vertexCount];
        var queue = new Queue<int>();
        queue.Enqueue(startNode);
        visited[startNode] = true;

        while (queue.Count > 0)
        {
            int current = queue.Dequeue();
            if (endNode != -1 && current == endNode)
            {
                result.Found = true;
                break;
            }

            foreach (var neighbor in adj[current])
            {
                int neighborId = neighbor.NeighborId;
                int edgeId = neighbor.EdgeId;
                if (edges[edgeId].Active && !visited[neighborId])
                {
                    visited[neighborId] = true;
                    result.ParentEdge[neighborId] = edgeId;
                    queue.Enqueue(neighborId);
                }
            }
        }

        if (endNode == -1) result.Found = true;
        return result;
    }

    private static int ReconstructPath(int startNode, int endNode, int[] parentEdge, int[] pathEdges)
    {
        int count = 0;
        int current = endNode;
        while (current != startNode && parentEdge[current] != -1)
        {
            int edgeId = parentEdge[current];
            pathEdges[count++] = edgeId;
            int u = edges[edgeId].U;
            int v = edges[edgeId].V;
            current = (current == u) ? v : u;
        }

        Array.Reverse(pathEdges, 0, count);
        return count;
    }

    private static long Solve()
    {
        int minCut = 3;
        int sourceNode = 0;
        int cutComponentSize = -1;

        for (int endNode = 1; endNode < vertexCount; endNode++)
        {
            foreach (var edge in edges) edge.Active = true;

            int pathsFound = 0;
            var parentEdge = new int[vertexCount];
            for (int k = 0; k < minCut; k++)
            {
                var bfsRes = BreadthFirstSearch(sourceNode, endNode);
                if (!bfsRes.Found) break;

                var pathEdges = new int[vertexCount];
                int pathLen = ReconstructPath(sourceNode, endNode, bfsRes.ParentEdge, pathEdges);
                for (int i = 0; i < pathLen; i++)
                {
                    edges[pathEdges[i]].Active = false;
                }
                pathsFound++;
            }

            if (pathsFound == minCut)
            {
                var connectivityCheck = BreadthFirstSearch(sourceNode, endNode);
                if (!connectivityCheck.Found)
                {
                    var componentBfs = BreadthFirstSearch(sourceNode, -1);
                    cutComponentSize = componentBfs.ParentEdge.Count(id => id != -1) + 1;
                    break;
                }
            }
        }

        if (cutComponentSize != -1)
        {
            long size1 = cutComponentSize;
            long size2 = vertexCount - size1;
            return size1 * size2;
        }

        return -1;
    }

    public static void Main(string[] args)
    {
        string[] lines = File.ReadAllLines("input.txt");
        foreach (var line in lines)
        {
            string[] parts = line.Split(new[] { ':' }, 2);
            if (parts.Length < 2) continue;

            int u = GetVertexId(parts[0].Trim());
            string[] neighbors = parts[1].Trim().Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            foreach (var neighbor in neighbors)
            {
                int v = GetVertexId(neighbor);
                AddEdge(u, v);
            }
        }

        long result = Solve();
        Console.WriteLine(result);
    }
}
