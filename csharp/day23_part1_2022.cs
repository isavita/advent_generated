
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public struct Point
{
    public int X { get; }
    public int Y { get; }

    public Point(int x, int y)
    {
        X = x;
        Y = y;
    }

    public override bool Equals(object obj)
    {
        if (obj is Point point)
        {
            return X == point.X && Y == point.Y;
        }

        return false;
    }

    public override int GetHashCode()
    {
        unchecked
        {
            return (X.GetHashCode() * 397) ^ Y.GetHashCode();
        }
    }

    public static bool operator ==(Point left, Point right)
    {
        return left.Equals(right);
    }

    public static bool operator !=(Point left, Point right)
    {
        return !left.Equals(right);
    }
}

public class Elf
{
    public Point Position { get; set; }
    public bool Moving { get; set; }
    public Point NextPosition { get; set; }

    public Elf(Point position)
    {
        Position = position;
    }
}

class Program
{
    private const int N = 1;
    private const int E = 3;
    private const int S = 5;
    private const int W = 7;

    private static readonly Point[] Dirs = new[]
    {
        new Point(-1, -1),
        new Point(-1, 0),
        new Point(-1, +1),
        new Point(0, +1),
        new Point(+1, +1),
        new Point(+1, 0),
        new Point(+1, -1),
        new Point(0, -1)
    };

    private static readonly int[] Order = { N, S, W, E };

    static void Main()
    {
        var map = new HashSet<Point>();
        var elves = new List<Elf>();
        int currDir = 0;

        Parse("input.txt", map, elves);

        for (int i = 0; i < 10; i++)
        {
            currDir = Run(map, elves, currDir, Order, Dirs);
        }

        var (min, max) = MinMax(map);

        int count = 0;
        for (int x = min.X; x <= max.X; x++)
        {
            for (int y = min.Y; y <= max.Y; y++)
            {
                if (!map.Contains(new Point(x, y)))
                {
                    count++;
                }
            }
        }

        Console.WriteLine(count);
    }

    static void Parse(string file, HashSet<Point> map, List<Elf> elves)
    {
        string[] lines = File.ReadAllLines(file);
        for (int row = 0; row < lines.Length; row++)
        {
            for (int col = 0; col < lines[row].Length; col++)
            {
                if (lines[row][col] == '#')
                {
                    var point = new Point(row, col);
                    map.Add(point);
                    elves.Add(new Elf(point));
                }
            }
        }
    }

    static bool AroundAllEmpty(Elf e, HashSet<Point> map, Point[] dirs)
    {
        foreach (var d in dirs)
        {
            var adj = new Point(e.Position.X + d.X, e.Position.Y + d.Y);
            if (map.Contains(adj))
            {
                return false;
            }
        }
        return true;
    }

    static bool ElfInDirection(Elf e, int wannaGo, HashSet<Point> map, Point[] dirs)
    {
        for (int j = -1; j <= 1; j++)
        {
            var dxy = dirs[(wannaGo + j + 8) % 8];
            var adj = new Point(e.Position.X + dxy.X, e.Position.Y + dxy.Y);
            if (map.Contains(adj))
            {
                return true;
            }
        }
        return false;
    }

    static int Run(HashSet<Point> map, List<Elf> elves, int currDir, int[] order, Point[] dirs)
    {
        var proposes = new Dictionary<Point, int>();

        foreach (var e in elves)
        {
            if (AroundAllEmpty(e, map, dirs))
            {
                continue;
            }

            for (int i = 0; i < 4; i++)
            {
                int dir = order[(currDir + i) % 4];

                if (ElfInDirection(e, dir, map, dirs))
                {
                    continue;
                }

                var dxy = dirs[dir];
                var dest = new Point(e.Position.X + dxy.X, e.Position.Y + dxy.Y);
                if (proposes.TryGetValue(dest, out int count))
                {
                    proposes[dest] = count + 1;
                }
                else
                {
                    proposes[dest] = 1;
                }

                e.NextPosition = dest;
                e.Moving = true;
                break;
            }
        }

        bool someoneMoved = false;
        foreach (var e in elves)
        {
            if (!e.Moving)
            {
                continue;
            }

            if (proposes.TryGetValue(e.NextPosition, out int count) && count > 1)
            {
                e.Moving = false;
                continue;
            }

            someoneMoved = true;
            map.Remove(e.Position);
            map.Add(e.NextPosition);
            e.Position = e.NextPosition;
            e.Moving = false;
        }

        return (currDir + 1) % 4;
    }

    static (Point, Point) MinMax(HashSet<Point> map)
    {
        int minX = int.MaxValue;
        int minY = int.MaxValue;
        int maxX = int.MinValue;
        int maxY = int.MinValue;

        foreach (var p in map)
        {
            minX = Math.Min(minX, p.X);
            minY = Math.Min(minY, p.Y);
            maxX = Math.Max(maxX, p.X);
            maxY = Math.Max(maxY, p.Y);
        }

        return (new Point(minX, minY), new Point(maxX, maxY));
    }
}
