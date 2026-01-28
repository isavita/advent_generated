
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class IntCode
{
    public Dictionary<long, long> Data = new Dictionary<long, long>();
    public long IP = 0, RelBase = 0;

    public IntCode(long[] program)
    {
        for (int i = 0; i < program.Length; i++) Data[i] = program[i];
    }

    long GetVal(long i, int mode)
    {
        long val = Data.GetValueOrDefault(IP + i);
        if (mode == 0) return Data.GetValueOrDefault(val);
        if (mode == 1) return val;
        return Data.GetValueOrDefault(RelBase + val);
    }

    void SetVal(long i, int mode, long val)
    {
        long target = Data.GetValueOrDefault(IP + i);
        if (mode == 0) Data[target] = val;
        else Data[RelBase + target] = val;
    }

    public long Run(long? input = null)
    {
        while (true)
        {
            long opCode = Data[IP] % 100;
            int m1 = (int)(Data[IP] / 100 % 10), m2 = (int)(Data[IP] / 1000 % 10), m3 = (int)(Data[IP] / 10000 % 10);
            if (opCode == 1) { SetVal(3, m3, GetVal(1, m1) + GetVal(2, m2)); IP += 4; }
            else if (opCode == 2) { SetVal(3, m3, GetVal(1, m1) * GetVal(2, m2)); IP += 4; }
            else if (opCode == 3) { if (!input.HasValue) return -1; SetVal(1, m1, input.Value); input = null; IP += 2; }
            else if (opCode == 4) { long res = GetVal(1, m1); IP += 2; return res; }
            else if (opCode == 5) { IP = GetVal(1, m1) != 0 ? GetVal(2, m2) : IP + 3; }
            else if (opCode == 6) { IP = GetVal(1, m1) == 0 ? GetVal(2, m2) : IP + 3; }
            else if (opCode == 7) { SetVal(3, m3, GetVal(1, m1) < GetVal(2, m2) ? 1 : 0); IP += 4; }
            else if (opCode == 8) { SetVal(3, m3, GetVal(1, m1) == GetVal(2, m2) ? 1 : 0); IP += 4; }
            else if (opCode == 9) { RelBase += GetVal(1, m1); IP += 2; }
            else return -2;
        }
    }
}

class Program
{
    static Dictionary<(int, int), int> grid = new Dictionary<(int, int), int>();
    static (int, int) oxygen;
    static int[] dx = { 0, 0, 0, -1, 1 }, dy = { 0, 1, -1, 0, 0 };
    static int[] opp = { 0, 2, 1, 4, 3 };

    static void Explore(IntCode robot, int x, int y)
    {
        for (int d = 1; d <= 4; d++)
        {
            int nx = x + dx[d], ny = y + dy[d];
            if (grid.ContainsKey((nx, ny))) continue;
            long status = robot.Run(d);
            grid[(nx, ny)] = (int)status;
            if (status != 0)
            {
                if (status == 2) oxygen = (nx, ny);
                Explore(robot, nx, ny);
                robot.Run(opp[d]);
            }
        }
    }

    static void Main()
    {
        var code = File.ReadAllText("input.txt").Split(',').Select(long.Parse).ToArray();
        grid[(0, 0)] = 1;
        Explore(new IntCode(code), 0, 0);

        var q = new Queue<((int, int) p, int d)>();
        q.Enqueue((oxygen, 0));
        var visited = new HashSet<(int, int)> { oxygen };
        int maxDist = 0;

        while (q.Count > 0)
        {
            var cur = q.Dequeue();
            maxDist = Math.Max(maxDist, cur.d);
            for (int d = 1; d <= 4; d++)
            {
                var np = (cur.p.Item1 + dx[d], cur.p.Item2 + dy[d]);
                if (grid.ContainsKey(np) && grid[np] != 0 && visited.Add(np))
                    q.Enqueue((np, cur.d + 1));
            }
        }
        Console.WriteLine(maxDist);
    }
}

