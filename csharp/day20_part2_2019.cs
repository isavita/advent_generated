
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class DonutMaze
{
    private class Point
    {
        public int X { get; set; }
        public int Y { get; set; }
        public int Level { get; set; }

        public override bool Equals(object obj)
        {
            var point = obj as Point;
            return point != null &&
                   X == point.X &&
                   Y == point.Y &&
                   Level == point.Level;
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hash = 17;
                hash = hash * 23 + X.GetHashCode();
                hash = hash * 23 + Y.GetHashCode();
                hash = hash * 23 + Level.GetHashCode();
                return hash;
            }
        }
    }

    public static void Main()
    {
        try
        {
            var maze = ReadMaze("input.txt");
            var portals = FindPortals(maze);
            var stepsPart1 = SolveMaze(maze, portals, false);
            var stepsPart2 = SolveMaze(maze, portals, true);

            Console.WriteLine(stepsPart1);
            Console.WriteLine(stepsPart2);
        }
        catch (IOException e)
        {
            Console.WriteLine(e.Message);
        }
    }

    private static char[][] ReadMaze(string filename)
    {
        var lines = File.ReadAllLines(filename);
        var maze = new char[lines.Length][];
        for (var i = 0; i < lines.Length; i++)
        {
            maze[i] = lines[i].ToCharArray();
        }
        return maze;
    }

    private static Dictionary<string, List<Point>> FindPortals(char[][] maze)
    {
        var portals = new Dictionary<string, List<Point>>();
        var rows = maze.Length;
        var cols = maze[0].Length;

        for (var i = 0; i < rows; i++)
        {
            for (var j = 0; j < cols; j++)
            {
                if (char.IsLetter(maze[i][j]))
                {
                    var label = new char[2];
                    label[0] = maze[i][j];
                    Point p = null;

                    if (j + 1 < cols && char.IsLetter(maze[i][j + 1]))
                    {
                        label[1] = maze[i][j + 1];
                        if (j + 2 < cols && maze[i][j + 2] == '.')
                        {
                            p = new Point { X = i, Y = j + 2, Level = 0 };
                        }
                        else if (j - 1 >= 0 && maze[i][j - 1] == '.')
                        {
                            p = new Point { X = i, Y = j - 1, Level = 0 };
                        }
                    }
                    else if (i + 1 < rows && char.IsLetter(maze[i + 1][j]))
                    {
                        label[1] = maze[i + 1][j];
                        if (i + 2 < rows && maze[i + 2][j] == '.')
                        {
                            p = new Point { X = i + 2, Y = j, Level = 0 };
                        }
                        else if (i - 1 >= 0 && maze[i - 1][j] == '.')
                        {
                            p = new Point { X = i - 1, Y = j, Level = 0 };
                        }
                    }

                    if (p != null)
                    {
                        var labelStr = new string(label);
                        if (!portals.TryGetValue(labelStr, out var list))
                        {
                            list = new List<Point>();
                            portals[labelStr] = list;
                        }
                        list.Add(p);
                    }
                }
            }
        }
        return portals;
    }

    private static int SolveMaze(char[][] maze, Dictionary<string, List<Point>> portals, bool recursive)
    {
        var start = portals["AA"][0];
        var end = portals["ZZ"][0];
        var queue = new Queue<Point>();
        queue.Enqueue(new Point { X = start.X, Y = start.Y, Level = 0 });
        var dist = new Dictionary<Point, int>();
        dist[new Point { X = start.X, Y = start.Y, Level = 0 }] = 0;

        var dx = new[] { 0, 0, 1, -1 };
        var dy = new[] { 1, -1, 0, 0 };

        while (queue.Count > 0)
        {
            var curr = queue.Dequeue();

            if (curr.X == end.X && curr.Y == end.Y && (!recursive || curr.Level == 0))
            {
                return dist[curr];
            }

            for (var i = 0; i < 4; i++)
            {
                var nx = curr.X + dx[i];
                var ny = curr.Y + dy[i];
                var next = new Point { X = nx, Y = ny, Level = curr.Level };

                if (nx >= 0 && nx < maze.Length && ny >= 0 && ny < maze[0].Length && maze[nx][ny] == '.')
                {
                    if (!dist.ContainsKey(next))
                    {
                        dist[next] = dist[curr] + 1;
                        queue.Enqueue(next);
                    }
                }
            }

            foreach (var entry in portals.Where(p => p.Value.Count == 2))
            {
                var points = entry.Value;
                var p1 = points[0];
                var p2 = points[1];
                Point portal = null;
                Point other = null;
                var newLevel = curr.Level;

                if (curr.X == p1.X && curr.Y == p1.Y)
                {
                    portal = p1;
                    other = p2;
                }
                else if (curr.X == p2.X && curr.Y == p2.Y)
                {
                    portal = p2;
                    other = p1;
                }

                if (portal != null)
                {
                    if (recursive)
                    {
                        if (IsOuter(portal, maze.Length, maze[0].Length))
                        {
                            newLevel--;
                        }
                        else
                        {
                            newLevel++;
                        }
                        if (newLevel < 0 || (newLevel == 0 && (entry.Key == "AA" || entry.Key == "ZZ")))
                            continue;
                    }
                    var next = new Point { X = other.X, Y = other.Y, Level = newLevel };
                    if (!dist.ContainsKey(next))
                    {
                        dist[next] = dist[curr] + 1;
                        queue.Enqueue(next);
                    }
                }
            }
        }
        return -1;
    }

    private static bool IsOuter(Point p, int rows, int cols)
    {
        return p.X <= 2 || p.X >= rows - 3 || p.Y <= 2 || p.Y >= cols - 3;
    }
}
