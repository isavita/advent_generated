using System;
using System.Collections.Generic;
using System.Linq;

struct Point3D : IEquatable<Point3D>
{
    public readonly int X;
    public readonly int Y;
    public readonly int Z;

    public Point3D(int x, int y, int z) { X = x; Y = y; Z = z; }

    public static Point3D operator -(Point3D a, Point3D b) => new Point3D(a.X - b.X, a.Y - b.Y, a.Z - b.Z);
    public static Point3D operator +(Point3D a, Point3D b) => new Point3D(a.X + b.X, a.Y + b.Y, a.Z + b.Z);

    public int ManhattanDistance(Point3D other) => Math.Abs(X - other.X) + Math.Abs(Y - other.Y) + Math.Abs(Z - other.Z);

    public Point3D Rotate(int r)
    {
        switch (r)
        {
            case 0: return new Point3D( X,  Y,  Z);
            case 1: return new Point3D( X, -Z,  Y);
            case 2: return new Point3D( X, -Y, -Z);
            case 3: return new Point3D( X,  Z, -Y);
            case 4: return new Point3D(-X, -Y,  Z);
            case 5: return new Point3D(-X, -Z, -Y);
            case 6: return new Point3D(-X,  Y, -Z);
            case 7: return new Point3D(-X,  Z,  Y);
            case 8: return new Point3D( Y, -X,  Z);
            case 9: return new Point3D( Y, -Z, -X);
            case 10: return new Point3D( Y,  X, -Z);
            case 11: return new Point3D( Y,  Z,  X);
            case 12: return new Point3D(-Y,  X,  Z);
            case 13: return new Point3D(-Y, -Z,  X);
            case 14: return new Point3D(-Y, -X, -Z);
            case 15: return new Point3D(-Y,  Z, -X);
            case 16: return new Point3D( Z,  Y, -X);
            case 17: return new Point3D( Z,  X,  Y);
            case 18: return new Point3D( Z, -Y,  X);
            case 19: return new Point3D( Z, -X, -Y);
            case 20: return new Point3D(-Z,  Y,  X);
            case 21: return new Point3D(-Z, -X,  Y);
            case 22: return new Point3D(-Z, -Y, -X);
            case 23: return new Point3D(-Z,  X, -Y);
            default: return new Point3D(X, Y, Z);
        }
    }

    public bool Equals(Point3D other) => X == other.X && Y == other.Y && Z == other.Z;
    public override bool Equals(object obj) => obj is Point3D other && Equals(other);
    public override int GetHashCode() => (X * 73856093) ^ (Y * 19349663) ^ (Z * 83492791);
}

class Program
{
    static List<List<Point3D>> ParseInput(string filename)
    {
        var lines = System.IO.File.ReadAllLines(filename);
        var scanners = new List<List<Point3D>>();
        var current = new List<Point3D>();
        foreach (var line in lines)
        {
            if (line.StartsWith("---"))
            {
                if (current.Count > 0)
                {
                    scanners.Add(current);
                    current = new List<Point3D>();
                }
            }
            else if (!string.IsNullOrWhiteSpace(line))
            {
                var parts = line.Split(',');
                int x = int.Parse(parts[0]);
                int y = int.Parse(parts[1]);
                int z = int.Parse(parts[2]);
                current.Add(new Point3D(x, y, z));
            }
        }
        if (current.Count > 0) scanners.Add(current);
        return scanners;
    }

    static bool FindOverlap(IEnumerable<Point3D> scanner1, IEnumerable<Point3D> scanner2, out List<Point3D> translatedScanner, out Point3D scannerPos)
    {
        translatedScanner = null;
        scannerPos = new Point3D(0, 0, 0);

        for (int rot = 0; rot < 24; rot++)
        {
            var rotated = scanner2.Select(p => p.Rotate(rot)).ToList();
            var diffCounts = new Dictionary<Point3D, int>();
            foreach (var p1 in scanner1)
            {
                foreach (var p2 in rotated)
                {
                    var diff = p1 - p2;
                    if (diffCounts.TryGetValue(diff, out int c)) diffCounts[diff] = c + 1;
                    else diffCounts[diff] = 1;
                }
            }

            int maxCount = 0;
            Point3D bestTranslation = new Point3D(0, 0, 0);
            foreach (var kv in diffCounts)
            {
                if (kv.Value > maxCount)
                {
                    maxCount = kv.Value;
                    bestTranslation = kv.Key;
                }
            }

            if (maxCount >= 12)
            {
                translatedScanner = rotated.Select(p => p + bestTranslation).ToList();
                scannerPos = bestTranslation;
                return true;
            }
        }
        return false;
    }

    static (HashSet<Point3D> beacons, HashSet<Point3D> scannerPositions) Solve(List<List<Point3D>> scanners)
    {
        var beacons = new HashSet<Point3D>(scanners[0]);
        var scannerPositions = new HashSet<Point3D> { new Point3D(0, 0, 0) };
        var remaining = new Queue<List<Point3D>>(scanners.Skip(1));

        while (remaining.Count > 0)
        {
            var scanner = remaining.Dequeue();
            if (FindOverlap(beacons, scanner, out var translated, out var pos))
            {
                foreach (var b in translated) beacons.Add(b);
                scannerPositions.Add(pos);
            }
            else
            {
                remaining.Enqueue(scanner);
            }
        }

        return (beacons, scannerPositions);
    }

    static void Main(string[] args)
    {
        var scanners = ParseInput("input.txt");
        var result = Solve(scanners);
        var beacons = result.beacons;
        var scannerPositions = result.scannerPositions;

        Console.WriteLine(beacons.Count);

        var list = scannerPositions.ToList();
        int maxDistance = 0;
        for (int i = 0; i < list.Count; i++)
        {
            for (int j = i + 1; j < list.Count; j++)
            {
                int d = list[i].ManhattanDistance(list[j]);
                if (d > maxDistance) maxDistance = d;
            }
        }
        Console.WriteLine(maxDistance);
    }
}