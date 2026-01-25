
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Coordinate
{
    public int X { get; set; }
    public int Y { get; set; }
    public int Z { get; set; }

    public override bool Equals(object obj)
    {
        var coord = obj as Coordinate;
        return coord != null && X == coord.X && Y == coord.Y && Z == coord.Z;
    }

    public override int GetHashCode()
    {
        unchecked
        {
            var hash = 17;
            hash = hash * 23 + X.GetHashCode();
            hash = hash * 23 + Y.GetHashCode();
            hash = hash * 23 + Z.GetHashCode();
            return hash;
        }
    }
}

public class Program
{
    private const int CYCLES = 6;
    private const int MAX_SIZE = 50;

    public static void Main(string[] args)
    {
        var activeCubes = ReadInput("input.txt");
        for (int cycle = 0; cycle < CYCLES; cycle++)
        {
            activeCubes = SimulateCycle(activeCubes);
        }
        Console.WriteLine(activeCubes.Count);
    }

    private static HashSet<Coordinate> ReadInput(string filename)
    {
        var activeCubes = new HashSet<Coordinate>();
        var lines = File.ReadAllLines(filename);
        for (int y = 0; y < lines.Length; y++)
        {
            for (int x = 0; x < lines[y].Length; x++)
            {
                if (lines[y][x] == '#')
                {
                    activeCubes.Add(new Coordinate { X = x, Y = y, Z = 0 });
                }
            }
        }
        return activeCubes;
    }

    private static HashSet<Coordinate> SimulateCycle(HashSet<Coordinate> activeCubes)
    {
        var newActiveCubes = new HashSet<Coordinate>();
        var neighborCounts = new Dictionary<Coordinate, int>();

        foreach (var cube in activeCubes)
        {
            for (int dz = -1; dz <= 1; dz++)
            {
                for (int dy = -1; dy <= 1; dy++)
                {
                    for (int dx = -1; dx <= 1; dx++)
                    {
                        if (dx == 0 && dy == 0 && dz == 0) continue;

                        var neighbor = new Coordinate { X = cube.X + dx, Y = cube.Y + dy, Z = cube.Z + dz };
                        if (neighborCounts.ContainsKey(neighbor))
                        {
                            neighborCounts[neighbor]++;
                        }
                        else
                        {
                            neighborCounts[neighbor] = 1;
                        }
                    }
                }
            }
        }

        foreach (var kvp in neighborCounts)
        {
            if (kvp.Value == 3 || (kvp.Value == 2 && activeCubes.Contains(kvp.Key)))
            {
                newActiveCubes.Add(kvp.Key);
            }
        }

        return newActiveCubes;
    }
}
