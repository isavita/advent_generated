
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public struct Coordinate
{
    public int Q { get; set; }
    public int R { get; set; }

    public override bool Equals(object obj)
    {
        if (obj is Coordinate other)
        {
            return Q == other.Q && R == other.R;
        }
        return false;
    }

    public override int GetHashCode()
    {
        unchecked
        {
            return (Q.GetHashCode() * 397) ^ R.GetHashCode();
        }
    }
}

public class HashTile
{
    public Coordinate Coord { get; set; }
    public bool IsBlack { get; set; }
}

public class Program
{
    private static readonly (int, int)[] Directions = new (int, int)[]
    {
        (1, 0), (0, 1), (-1, 1), (-1, 0), (0, -1), (1, -1)
    };

    private static readonly Dictionary<string, int> DirMap = new Dictionary<string, int>()
    {
        {"e", 0}, {"se", 1}, {"sw", 2}, {"w", 3}, {"nw", 4}, {"ne", 5}
    };

    public static void Main()
    {
        var blackTiles = new HashSet<Coordinate>();

        foreach (var line in File.ReadLines("input.txt"))
        {
            var coord = new Coordinate { Q = 0, R = 0 };
            for (int i = 0; i < line.Length;)
            {
                string dir;
                if (line[i] == 'e' || line[i] == 'w')
                {
                    dir = line[i].ToString();
                    i++;
                }
                else
                {
                    dir = line.Substring(i, 2);
                    i += 2;
                }

                var dirIndex = DirMap[dir];
                coord.Q += Directions[dirIndex].Item1;
                coord.R += Directions[dirIndex].Item2;
            }

            if (blackTiles.Contains(coord))
                blackTiles.Remove(coord);
            else
                blackTiles.Add(coord);
        }

        for (int day = 0; day < 100; day++)
        {
            var tilesToCheck = new HashSet<Coordinate>();
            foreach (var tile in blackTiles)
            {
                tilesToCheck.Add(tile);
                foreach (var neighbor in GetNeighbors(tile))
                {
                    tilesToCheck.Add(neighbor);
                }
            }

            var newBlackTiles = new HashSet<Coordinate>();
            foreach (var tile in tilesToCheck)
            {
                var blackNeighbors = GetNeighbors(tile).Count(neighbor => blackTiles.Contains(neighbor));
                if ((blackTiles.Contains(tile) && (blackNeighbors == 1 || blackNeighbors == 2)) ||
                    (!blackTiles.Contains(tile) && blackNeighbors == 2))
                {
                    newBlackTiles.Add(tile);
                }
            }

            blackTiles = newBlackTiles;
        }

        Console.WriteLine(blackTiles.Count);
    }

    private static IEnumerable<Coordinate> GetNeighbors(Coordinate tile)
    {
        foreach (var dir in Directions)
        {
            yield return new Coordinate { Q = tile.Q + dir.Item1, R = tile.R + dir.Item2 };
        }
    }
}
