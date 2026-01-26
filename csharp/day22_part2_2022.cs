using System;
using System.IO;
using System.Collections.Generic;

struct P : IEquatable<P>
{
    public int X, Y;
    public P(int x, int y) { X = x; Y = y; }
    public bool Equals(P other) => X == other.X && Y == other.Y;
    public override bool Equals(object obj) => obj is P p && Equals(p);
    public override int GetHashCode() => X * 31 + Y;
}

enum Dir { N, E, S, W }

static class DirExt
{
    public static Dir Rotate(this Dir d, char c) => c == 'R' ? (Dir)(((int)d + 1) & 3) : (Dir)(((int)d + 3) & 3);
    public static int Points(this Dir d) => ((int)d + 3) & 3;
}

class Movement
{
    public int Steps;
    public char? Rotate;
    public Movement(int steps, char? rot) { Steps = steps; Rotate = rot; }
}

class Human
{
    public P Curr;
    public Dir Facing;
    public Human(P start, Dir dir) { Curr = start; Facing = dir; }
    public (P pos, Dir dir) Walk(Dictionary<P, bool> map, P[] dirs, int size)
    {
        var delta = dirs[(int)Facing];
        var next = new P(Curr.X + delta.X, Curr.Y + delta.Y);
        if (map.TryGetValue(next, out bool wall))
        {
            return wall ? (Curr, Facing) : (next, Facing);
        }
        var (crossPos, crossDir) = CrossBorder(next, Facing, size);
        if (map.TryGetValue(crossPos, out bool crossWall) && crossWall) return (Curr, Facing);
        return (crossPos, crossDir);
    }
    static (P pos, Dir dir) CrossBorder(P n, Dir dir, int S)
    {
        int x = n.X, y = n.Y, s = S;
        if (x == -1 && y < 2 * s) return (new P(y + 2 * s, x + 1), Dir.E);
        if (x == -1 && y >= 2 * s) return (new P(x + 4 * s, y - 2 * s), Dir.N);
        if (x == s && dir == Dir.S) return (new P(y - s, x + s - 1), Dir.W);
        if (x == 2 * s - 1 && dir == Dir.N) return (new P(y + s, x - s + 1), Dir.E);
        if (x == 3 * s && dir == Dir.S) return (new P(y + 2 * s, x - 2 * s - 1), Dir.W);
        if (x == 4 * s) return (new P(x - 4 * s, y + 2 * s), Dir.S);
        if (y == -1 && x < 3 * s) return (new P(3 * s - 1 - x, y + s + 1), Dir.E);
        if (y == -1 && x >= 3 * s) return (new P(y + 1, x - 2 * s), Dir.S);
        if (y == s - 1 && x < s) return (new P(3 * s - 1 - x, y - s + 1), Dir.E);
        if (y == s - 1 && x >= s && dir == Dir.W) return (new P(y + s + 1, x - s), Dir.S);
        if (y == s && dir == Dir.E) return (new P(y + 2 * s - 1, x - 2 * s), Dir.N);
        if (y == 2 * s && x < 2 * s && dir == Dir.E) return (new P(y - s - 1, x + s), Dir.N);
        if (y == 2 * s && x >= 2 * s) return (new P(3 * s - 1 - x, y + s - 1), Dir.W);
        if (y == 3 * s) return (new P(3 * s - 1 - x, y - s - 1), Dir.W);
        throw new Exception();
    }
}

class Program
{
    static List<Movement> ParsePath(string path)
    {
        var list = new List<Movement>();
        int acc = 0;
        foreach (char c in path)
        {
            if (c == 'R' || c == 'L')
            {
                if (acc != 0) { list.Add(new Movement(acc, null)); acc = 0; }
                list.Add(new Movement(0, c));
            }
            else acc = acc * 10 + (c - '0');
        }
        if (acc != 0) list.Add(new Movement(acc, null));
        return list;
    }

    static (Dictionary<P, bool> map, int size, List<Movement> moves) ParseInput(string file)
    {
        var map = new Dictionary<P, bool>();
        int size = 0;
        var moves = new List<Movement>();
        var lines = File.ReadAllLines(file);
        int r = 0;
        while (r < lines.Length && !string.IsNullOrWhiteSpace(lines[r]))
        {
            var line = lines[r];
            if (r == 0) size = line.Length / 3;
            for (int c = 0; c < line.Length; c++)
            {
                char ch = line[c];
                if (ch == ' ') continue;
                map[new P(r, c)] = ch == '#';
            }
            r++;
        }
        var movementLine = lines[r + 1].Trim();
        moves = ParsePath(movementLine);
        return (map, size, moves);
    }

    static void Main()
    {
        var (map, size, moves) = ParseInput("input.txt");
        var dirs = new[] { new P(-1, 0), new P(0, 1), new P(1, 0), new P(0, -1) };
        var human = new Human(new P(0, size), Dir.E);
        foreach (var mv in moves)
        {
            if (mv.Rotate.HasValue) human.Facing = human.Facing.Rotate(mv.Rotate.Value);
            for (int i = 0; i < mv.Steps; i++)
            {
                var (pos, dir) = human.Walk(map, dirs, size);
                if (pos.Equals(human.Curr) && dir == human.Facing) break;
                human.Curr = pos;
                human.Facing = dir;
            }
        }
        int result = 1000 * (human.Curr.X + 1) + 4 * (human.Curr.Y + 1) + human.Facing.Points();
        Console.WriteLine(result);
    }
}