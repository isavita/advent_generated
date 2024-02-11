
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        int n = CleaningRobot(input);
        Console.WriteLine(n);
    }

    static int CleaningRobot(string input)
    {
        var grid = input.Split("\n").Select(line => line.ToCharArray()).ToArray();
        List<int[]> dirs = new List<int[]> { new int[] { 0, -1 }, new int[] { 0, 1 }, new int[] { 1, 0 }, new int[] { -1, 0 } };

        List<int[]> GetEdgeWeights(char[][] grid, int[] start)
        {
            Dictionary<string, int> poiToDistance = new Dictionary<string, int> { { grid[start[0]][start[1]].ToString(), 0 } };
            Queue<int[]> queue = new Queue<int[]>();
            queue.Enqueue(new int[] { start[0], start[1], 0 });
            HashSet<int[]> visited = new HashSet<int[]>(new MyEqualityComparer());

            while (queue.Count > 0)
            {
                int[] front = queue.Dequeue();

                if (visited.Contains(new int[] { front[0], front[1] }))
                    continue;

                visited.Add(new int[] { front[0], front[1] });

                if (Regex.IsMatch(grid[front[0]][front[1]].ToString(), "[0-9]"))
                    poiToDistance[grid[front[0]][front[1]].ToString()] = front[2];

                foreach (var d in dirs)
                {
                    int nextRow = front[0] + d[0];
                    int nextCol = front[1] + d[1];

                    if (grid[nextRow][nextCol] != '#')
                        queue.Enqueue(new int[] { nextRow, nextCol, front[2] + 1 });
                }
            }

            List<int[]> distances = new List<int[]>();
            foreach (var kvp in poiToDistance)
            {
                int n = int.Parse(kvp.Key);
                distances.Add(new int[] { n, kvp.Value });
            }

            return distances;
        }

        List<int[]>[] graph = new List<int[]>[10];
        for (int r = 0; r < grid.Length; r++)
        {
            for (int c = 0; c < grid[r].Length; c++)
            {
                if (Regex.IsMatch(grid[r][c].ToString(), "[0-9]"))
                {
                    string poi = grid[r][c].ToString();
                    List<int[]> distancesFromPOI = GetEdgeWeights(grid, new int[] { r, c });

                    int index = int.Parse(poi);
                    graph[index] = distancesFromPOI;
                }
            }
        }

        return DFS(graph, 0, new HashSet<int> { 0 }, true);
    }

    static int DFS(List<int[]>[] graph, int entryIndex, HashSet<int> visited, bool returnToZero)
    {
        if (graph.Length == visited.Count)
        {
            if (returnToZero)
                return graph[entryIndex][0][1];
            return 0;
        }

        int minDistance = int.MaxValue;
        foreach (var edge in graph[entryIndex])
        {
            int i = edge[0];
            int val = edge[1];
            if (!visited.Contains(i))
            {
                visited.Add(i);
                int dist = val + DFS(graph, i, visited, returnToZero);
                minDistance = Math.Min(minDistance, dist);
                visited.Remove(i);
            }
        }

        return minDistance;
    }

    class MyEqualityComparer : IEqualityComparer<int[]>
    {
        public bool Equals(int[] x, int[] y)
        {
            return x[0] == y[0] && x[1] == y[1];
        }

        public int GetHashCode(int[] obj)
        {
            return obj[0].GetHashCode() ^ obj[1].GetHashCode();
        }
    }
}
