
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Monkey
{
    public List<long> Items;
    public Func<long, long> Op;
    public int TestDivisor;
    public int TrueTarget;
    public int FalseTarget;
    public long InspectionCount;
    public Monkey(List<long> items, Func<long, long> op, int testDivisor, int trueTarget, int falseTarget)
    {
        Items = items;
        Op = op;
        TestDivisor = testDivisor;
        TrueTarget = trueTarget;
        FalseTarget = falseTarget;
        InspectionCount = 0;
    }
}

class Program
{
    static List<Monkey> ReadMonkeys(string file)
    {
        var monkeys = new List<Monkey>();
        var lines = File.ReadAllLines(file);
        for (int i = 0; i < lines.Length; i++)
        {
            if (!lines[i].StartsWith("Monkey")) continue;
            var items = lines[++i].Trim().Substring("Starting items: ".Length)
                .Split(", ", StringSplitOptions.RemoveEmptyEntries)
                .Select(long.Parse).ToList();
            var opLine = lines[++i].Trim().Substring("Operation: new = ".Length);
            var op = CreateOp(opLine);
            var testDiv = int.Parse(lines[++i].Trim().Substring("Test: divisible by ".Length));
            var trueT = int.Parse(lines[++i].Trim().Substring("If true: throw to monkey ".Length));
            var falseT = int.Parse(lines[++i].Trim().Substring("If false: throw to monkey ".Length));
            monkeys.Add(new Monkey(items, op, testDiv, trueT, falseT));
        }
        return monkeys;
    }

    static Func<long, long> CreateOp(string s)
    {
        var p = s.Split(' ');
        var oper = p[1];
        var valStr = p[2];
        return old =>
        {
            long v = valStr == "old" ? old : long.Parse(valStr);
            return oper == "+" ? old + v : old * v;
        };
    }

    static long Gcd(long a, long b)
    {
        while (b != 0) { var t = b; b = a % b; a = t; }
        return a;
    }

    static long Solve(List<Monkey> monkeys, int rounds, bool divideByThree)
    {
        long lcm = 1;
        foreach (var m in monkeys) lcm = lcm / Gcd(lcm, m.TestDivisor) * m.TestDivisor;
        for (int r = 0; r < rounds; r++)
        {
            foreach (var m in monkeys)
            {
                foreach (var item in m.Items)
                {
                    m.InspectionCount++;
                    var w = m.Op(item);
                    w = divideByThree ? w / 3 : w % lcm;
                    var target = w % m.TestDivisor == 0 ? m.TrueTarget : m.FalseTarget;
                    monkeys[target].Items.Add(w);
                }
                m.Items.Clear();
            }
        }
        return monkeys.Select(m => m.InspectionCount).OrderByDescending(x => x).Take(2).Aggregate(1L, (a, b) => a * b);
    }

    static void Main()
    {
        var monkeys1 = ReadMonkeys("input.txt");
        var part1 = Solve(monkeys1, 20, true);
        Console.WriteLine(part1);
        var monkeys2 = ReadMonkeys("input.txt");
        var part2 = Solve(monkeys2, 10000, false);
        Console.WriteLine(part2);
    }
}
