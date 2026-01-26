
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class Elf
{
    public int X { get; set; }
    public int Y { get; set; }
    public bool Moving { get; set; }
    public int NextX { get; set; }
    public int NextY { get; set; }

    public Elf(int x, int y)
    {
        X = x;
        Y = y;
    }
}

public class ElfSimulation
{
    private static readonly int[][] Dirs = new int[][] { new int[] { -1, -1 }, new int[] { -1, 0 }, new int[] { -1, 1 }, new int[] { 0, 1 }, new int[] { 1, 1 }, new int[] { 1, 0 }, new int[] { 1, -1 }, new int[] { 0, -1 } };
    private static readonly int[] Order = new int[] { 1, 5, 7, 3 };

    private static bool AroundAllEmpty(Elf e, Dictionary<int, Elf> map)
    {
        foreach (var dir in Dirs)
        {
            if (map.ContainsKey(Hash(e.X + dir[0], e.Y + dir[1])))
            {
                return false;
            }
        }
        return true;
    }

    private static bool ElfInDirection(Elf e, int wannaGo, Dictionary<int, Elf> map)
    {
        for (int j = -1; j <= 1; j++)
        {
            int dirIndex = (wannaGo + j + 8) % 8;
            var dir = Dirs[dirIndex];
            if (map.ContainsKey(Hash(e.X + dir[0], e.Y + dir[1])))
            {
                return true;
            }
        }
        return false;
    }

    private static int Hash(int x, int y)
    {
        return x * 10000 + y;
    }

    private static bool Run(List<Elf> elves, Dictionary<int, Elf> map, int currDir)
    {
        var proposes = new Dictionary<int, int>();
        foreach (var elf in elves)
        {
            if (AroundAllEmpty(elf, map))
            {
                continue;
            }
            for (int i = 0; i < 4; i++)
            {
                int dir_ = Order[(currDir + i) % 4];
                if (ElfInDirection(elf, dir_, map))
                {
                    continue;
                }
                var dxy = Dirs[dir_];
                int destX = elf.X + dxy[0];
                int destY = elf.Y + dxy[1];
                int destHash = Hash(destX, destY);
                proposes[destHash] = proposes.TryGetValue(destHash, out int count) ? count + 1 : 1;
                elf.NextX = destX;
                elf.NextY = destY;
                elf.Moving = true;
                break;
            }
        }

        bool someoneMoved = false;
        foreach (var elf in elves)
        {
            if (!elf.Moving)
            {
                continue;
            }
            int nextHash = Hash(elf.NextX, elf.NextY);
            if (proposes[nextHash] > 1)
            {
                elf.Moving = false;
                continue;
            }
            someoneMoved = true;
            map.Remove(Hash(elf.X, elf.Y));
            elf.X = elf.NextX;
            elf.Y = elf.NextY;
            map[Hash(elf.X, elf.Y)] = elf;
            elf.Moving = false;
        }

        return someoneMoved;
    }

    public static void Main(string[] args)
    {
        int currDir = 0;
        var elves = new List<Elf>();
        var map = new Dictionary<int, Elf>();

        var lines = File.ReadAllLines("input.txt");
        for (int row = 0; row < lines.Length; row++)
        {
            var line = lines[row];
            for (int col = 0; col < line.Length; col++)
            {
                if (line[col] == '#')
                {
                    var elf = new Elf(row, col);
                    elves.Add(elf);
                    map[Hash(row, col)] = elf;
                }
            }
        }

        int i = 0;
        while (true)
        {
            if (!Run(elves, map, currDir))
            {
                Console.WriteLine(i + 1);
                break;
            }
            currDir = (currDir + 1) % 4;
            i++;
        }
    }
}
