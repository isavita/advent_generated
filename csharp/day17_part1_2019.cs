using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

struct Point : IEquatable<Point>
{
    public int X, Y;
    public Point(int x, int y) { X = x; Y = y; }
    public bool Equals(Point other) => X == other.X && Y == other.Y;
    public override bool Equals(object obj) => obj is Point p && Equals(p);
    public override int GetHashCode() => X * 397 ^ Y;
}

class Machine
{
    readonly Dictionary<long, long> mem = new Dictionary<long, long>();
    long ip, rel;
    readonly Queue<long> input;
    readonly List<long> output;
    public Machine(IEnumerable<long> prog, Queue<long> inQ, List<long> outL)
    {
        long i = 0;
        foreach (var v in prog) mem[i++] = v;
        ip = 0; rel = 0; input = inQ; output = outL;
    }
    long Get(long addr, int mode)
    {
        long a = mode == 0 ? (mem.ContainsKey(addr) ? mem[addr] : 0) :
                 mode == 1 ? addr :
                 rel + (mem.ContainsKey(addr) ? mem[addr] : 0);
        return mem.ContainsKey(a) ? mem[a] : 0;
    }
    void Set(long addr, int mode, long val)
    {
        long a = mode == 0 ? (mem.ContainsKey(addr) ? mem[addr] : 0) :
                 rel + (mem.ContainsKey(addr) ? mem[addr] : 0);
        mem[a] = val;
    }
    public void Run()
    {
        while (true)
        {
            long instr = mem.ContainsKey(ip) ? mem[ip] : 0;
            int op = (int)(instr % 100);
            int[] m = { (int)(instr / 100 % 10), (int)(instr / 1000 % 10), (int)(instr / 10000 % 10) };
            if (op == 99) break;
            switch (op)
            {
                case 1:
                    Set(ip + 3, m[2], Get(ip + 1, m[0]) + Get(ip + 2, m[1]));
                    ip += 4; break;
                case 2:
                    Set(ip + 3, m[2], Get(ip + 1, m[0]) * Get(ip + 2, m[1]));
                    ip += 4; break;
                case 3:
                    Set(ip + 1, m[0], input.Dequeue());
                    ip += 2; break;
                case 4:
                    output.Add(Get(ip + 1, m[0]));
                    ip += 2; break;
                case 5:
                    ip = Get(ip + 1, m[0]) != 0 ? Get(ip + 2, m[1]) : ip + 3;
                    break;
                case 6:
                    ip = Get(ip + 1, m[0]) == 0 ? Get(ip + 2, m[1]) : ip + 3;
                    break;
                case 7:
                    Set(ip + 3, m[2], Get(ip + 1, m[0]) < Get(ip + 2, m[1]) ? 1 : 0);
                    ip += 4; break;
                case 8:
                    Set(ip + 3, m[2], Get(ip + 1, m[0]) == Get(ip + 2, m[1]) ? 1 : 0);
                    ip += 4; break;
                case 9:
                    rel += Get(ip + 1, m[0]);
                    ip += 2; break;
                default: return;
            }
        }
    }
}

static class Program
{
    static void Main()
    {
        var line = File.ReadAllText("input.txt").Trim();
        var prog = line.Split(',').Select(long.Parse);
        var outVals = new List<long>();
        var machine = new Machine(prog, new Queue<long>(), outVals);
        machine.Run();

        var scaff = new HashSet<Point>();
        Point robot = new Point();
        int dir = 0, x = 0, y = 0;
        foreach (var v in outVals)
        {
            char c = (char)v;
            if (c == '\n')
            {
                y++; x = 0; continue;
            }
            switch (c)
            {
                case '^': robot = new Point(x, y); dir = 0; scaff.Add(robot); break;
                case '>': robot = new Point(x, y); dir = 1; scaff.Add(robot); break;
                case 'v': robot = new Point(x, y); dir = 2; scaff.Add(robot); break;
                case '<': robot = new Point(x, y); dir = 3; scaff.Add(robot); break;
                case '#': scaff.Add(new Point(x, y)); break;
            }
            x++;
        }

        long sum = 0;
        int[] dx = { 0, 0, 1, -1 };
        int[] dy = { 1, -1, 0, 0 };
        foreach (var p in scaff)
        {
            bool inter = true;
            for (int i = 0; i < 4; i++)
                if (!scaff.Contains(new Point(p.X + dx[i], p.Y + dy[i]))) { inter = false; break; }
            if (inter) sum += (long)p.X * p.Y;
        }
        Console.WriteLine(sum);
    }
}