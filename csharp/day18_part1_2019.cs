
using System;
using System.IO;
using System.Collections.Generic;

class Point
{
    public int x, y;
}

class State
{
    public Point pos;
    public int keys;

    public override bool Equals(object obj)
    {
        State other = obj as State;
        return pos.x == other.pos.x && pos.y == other.pos.y && keys == other.keys;
    }

    public override int GetHashCode()
    {
        return pos.x.GetHashCode() ^ pos.y.GetHashCode() ^ keys.GetHashCode();
    }
}

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        List<string> grid = new List<string>();
        Point start = new Point();
        Dictionary<char, int> keyMap = new Dictionary<char, int>();
        int keyCounter = 0;

        for (int y = 0; y < lines.Length; y++)
        {
            string line = lines[y];
            grid.Add(line);
            for (int x = 0; x < line.Length; x++)
            {
                if (line[x] == '@')
                {
                    start.x = x;
                    start.y = y;
                }
                else if (line[x] >= 'a' && line[x] <= 'z')
                {
                    keyMap[line[x]] = keyCounter;
                    keyCounter++;
                }
            }
        }

        Console.WriteLine(FindShortestPath(grid, start, keyMap));
    }

    static int FindShortestPath(List<string> grid, Point start, Dictionary<char, int> keyMap)
    {
        Point[] dirs = { new Point { x = 0, y = -1 }, new Point { x = -1, y = 0 }, new Point { x = 0, y = 1 }, new Point { x = 1, y = 0 } };
        Dictionary<State, bool> visited = new Dictionary<State, bool>();
        Queue<State> queue = new Queue<State>();
        queue.Enqueue(new State { pos = start, keys = 0 });
        int steps = 0;

        while (queue.Count > 0)
        {
            int size = queue.Count;
            for (int i = 0; i < size; i++)
            {
                State current = queue.Dequeue();

                if (current.keys == (1 << keyMap.Count) - 1)
                {
                    return steps;
                }

                foreach (Point d in dirs)
                {
                    Point next = new Point { x = current.pos.x + d.x, y = current.pos.y + d.y };
                    if (next.x >= 0 && next.x < grid[0].Length && next.y >= 0 && next.y < grid.Count)
                    {
                        char c = grid[next.y][next.x];
                        if (c != '#' && !(c >= 'A' && c <= 'Z' && (current.keys & (1 << keyMap[Char.ToLower(c)])) == 0))
                        {
                            State newState = new State { pos = next, keys = current.keys };
                            if (c >= 'a' && c <= 'z')
                            {
                                newState.keys |= 1 << keyMap[c];
                            }
                            if (!visited.ContainsKey(newState))
                            {
                                visited[newState] = true;
                                queue.Enqueue(newState);
                            }
                        }
                    }
                }
            }
            steps++;
        }
        return -1;
    }
}
