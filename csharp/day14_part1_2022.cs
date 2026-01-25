
using System;
using System.IO;
using System.Linq;

public class Program
{
    private const int MaxPoints = 100000;
    private const int MaxGridSize = 1000;
    private const int InitialSandX = 500;

    private static Point[] _points = new Point[MaxPoints];
    private static bool[,] _grid = new bool[MaxGridSize, MaxGridSize];
    private static int _pointsSize;
    private static int _floorLevel;

    public struct Point
    {
        public int X;
        public int Y;

        public Point(int x, int y)
        {
            X = x;
            Y = y;
        }
    }

    public static void Main(string[] args)
    {
        var input = File.ReadAllLines("input.txt");
        _pointsSize = 0;
        foreach (var line in input)
        {
            var tokens = line.Split(new[] { " -> " }, StringSplitOptions.None);
            var current = ParsePoint(tokens[0]);
            _points[_pointsSize++] = current;
            for (int i = 1; i < tokens.Length; i++)
            {
                var next = ParsePoint(tokens[i]);
                PopulateGrid(current, next);
                current = next;
                _points[_pointsSize++] = current;
            }
        }

        Console.WriteLine(Fill());
    }

    private static Point ParsePoint(string token)
    {
        var parts = token.Split(',');
        return new Point(int.Parse(parts[0]), int.Parse(parts[1]));
    }

    private static void PopulateGrid(Point p1, Point p2)
    {
        if (p1.X == p2.X)
        {
            var minY = Math.Min(p1.Y, p2.Y);
            var maxY = Math.Max(p1.Y, p2.Y);
            for (int y = minY; y <= maxY; y++)
            {
                _grid[p1.X, y] = true;
                if (y > _floorLevel) _floorLevel = y;
            }
        }
        else
        {
            var minX = Math.Min(p1.X, p2.X);
            var maxX = Math.Max(p1.X, p2.X);
            for (int x = minX; x <= maxX; x++)
            {
                _grid[x, p1.Y] = true;
                if (p1.Y > _floorLevel) _floorLevel = p1.Y;
            }
        }
    }

    private static int Fill()
    {
        _floorLevel++;
        int sands = 0, firstFloorTouch = 0;
        while (!_grid[InitialSandX, 0])
        {
            var sand = new Point(InitialSandX, 0);
            bool settled = false;
            while (!settled)
            {
                var nextSand = new[]
                {
                    new Point(sand.X, sand.Y + 1),
                    new Point(sand.X - 1, sand.Y + 1),
                    new Point(sand.X + 1, sand.Y + 1)
                };

                var moved = false;
                foreach (var ns in nextSand)
                {
                    if (ns.X >= 0 && ns.X < MaxGridSize && ns.Y >= 0 && ns.Y < MaxGridSize && !_grid[ns.X, ns.Y])
                    {
                        sand = ns;
                        moved = true;
                        break;
                    }
                }

                if (!moved)
                {
                    _grid[sand.X, sand.Y] = true;
                    settled = true;
                }

                if (sand.Y == _floorLevel)
                {
                    if (firstFloorTouch == 0)
                    {
                        firstFloorTouch = sands;
                    }
                    _grid[sand.X, sand.Y] = true;
                    settled = true;
                }
            }
            sands++;
        }
        return firstFloorTouch;
    }
}
