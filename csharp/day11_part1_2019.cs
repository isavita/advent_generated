using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var p = File.ReadAllText("input.txt").Trim().Split(',').Select(long.Parse).ToArray();
        var g = new Dictionary<(int, int), int>();
        var v = new VM(p);
        int x = 0, y = 0, d = 0;
        while (!v.Halt)
        {
            g.TryGetValue((x, y), out int c);
            v.In.Enqueue(c);
            v.Run();
            if (v.Out.Count == 2)
            {
                g[(x, y)] = (int)v.Out[0];
                d = (d + (v.Out[1] == 0 ? 3 : 1)) % 4;
                if (d == 0) y--; else if (d == 1) x++; else if (d == 2) y++; else x--;
            }
        }
        Console.WriteLine(g.Count);
    }
}

class VM
{
    public Dictionary<long, long> M = new Dictionary<long, long>();
    public long IP = 0;
    public Queue<long> In = new Queue<long>();
    public List<long> Out = new List<long>();
    public bool Halt = false;
    public VM(long[] p) { for (int i = 0; i < p.Length; i++) M[i] = p[i]; }
    long G(long a) => M.TryGetValue(a, out long val) ? val : 0;
    long A(int i) => (G(IP) / (i == 1 ? 100 : i == 2 ? 1000 : 10000)) % 10 == 1 ? IP + i : G(IP + i);
    public void Run()
    {
        Out.Clear();
        while (true)
        {
            long op = G(IP) % 100;
            if (op == 99) { Halt = true; return; }
            if (op == 1 || op == 2 || op == 7 || op == 8)
            {
                long v1 = G(A(1)), v2 = G(A(2)), dst = A(3);
                M[dst] = op == 1 ? v1 + v2 : op == 2 ? v1 * v2 : (op == 7 ? (v1 < v2 ? 1 : 0) : (v1 == v2 ? 1 : 0));
                IP += 4;
            }
            else if (op == 3)
            {
                if (In.Count == 0) return;
                M[A(1)] = In.Dequeue(); IP += 2;
            }
            else if (op == 4)
            {
                Out.Add(G(A(1))); IP += 2;
            }
            else if (op == 5 || op == 6)
            {
                long v1 = G(A(1)), v2 = G(A(2));
                if ((op == 5 && v1 != 0) || (op == 6 && v1 == 0)) IP = v2; else IP += 3;
            }
        }
    }
}