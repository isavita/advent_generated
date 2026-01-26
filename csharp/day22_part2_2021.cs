using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        var input = File.ReadAllText("input.txt").Trim();
        Console.WriteLine(Solve(input));
    }

    static long Solve(string input)
    {
        var cubes = Parse(input);
        var finalList = new List<Cube>();
        foreach (var c in cubes)
        {
            var toAdd = new List<Cube>();
            foreach (var f in finalList)
            {
                var inter = f.Intersect(c);
                if (inter != null) toAdd.Add(inter.Value);
            }
            if (c.IsOn) toAdd.Add(c);
            finalList.AddRange(toAdd);
        }
        long total = 0;
        foreach (var c in finalList) total += c.Volume();
        return total;
    }

    struct Cube
    {
        public bool IsOn;
        public int X1, X2, Y1, Y2, Z1, Z2;
        public Cube(bool on, int x1, int x2, int y1, int y2, int z1, int z2)
        {
            IsOn = on; X1 = x1; X2 = x2; Y1 = y1; Y2 = y2; Z1 = z1; Z2 = z2;
        }
        public Cube? Intersect(Cube other)
        {
            int x1 = Math.Max(X1, other.X1), x2 = Math.Min(X2, other.X2);
            int y1 = Math.Max(Y1, other.Y1), y2 = Math.Min(Y2, other.Y2);
            int z1 = Math.Max(Z1, other.Z1), z2 = Math.Min(Z2, other.Z2);
            if (x1 > x2 || y1 > y2 || z1 > z2) return null;
            bool state = IsOn == other.IsOn ? !IsOn : other.IsOn;
            return new Cube(state, x1, x2, y1, y2, z1, z2);
        }
        public long Volume()
        {
            long vol = (long)(X2 - X1 + 1) * (Y2 - Y1 + 1) * (Z2 - Z1 + 1);
            return IsOn ? vol : -vol;
        }
    }

    static List<Cube> Parse(string input)
    {
        var list = new List<Cube>();
        var regex = new Regex(@"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)");
        foreach (Match m in regex.Matches(input))
        {
            bool on = m.Groups[1].Value == "on";
            int x1 = int.Parse(m.Groups[2].Value);
            int x2 = int.Parse(m.Groups[3].Value);
            int y1 = int.Parse(m.Groups[4].Value);
            int y2 = int.Parse(m.Groups[5].Value);
            int z1 = int.Parse(m.Groups[6].Value);
            int z2 = int.Parse(m.Groups[7].Value);
            list.Add(new Cube(on, x1, x2, y1, y2, z1, z2));
        }
        return list;
    }
}
