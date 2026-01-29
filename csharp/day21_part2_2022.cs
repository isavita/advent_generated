
using System;
using System.IO;
using System.Collections.Generic;

class Monkey
{
    public string Name;
    public long Val;
    public bool HasVal;
    public Monkey Left;
    public Monkey Right;
    public char Op;
}

class Program
{
    static Dictionary<string, Monkey> map = new Dictionary<string, Monkey>();

    static Monkey FindOrCreate(string name)
    {
        if (map.TryGetValue(name, out var m)) return m;
        m = new Monkey { Name = name, Val = 0, HasVal = false, Left = null, Right = null, Op = '\0' };
        map[name] = m;
        return m;
    }

    static bool TrySolve(Monkey m, out long result)
    {
        result = 0;
        if (m == null) return false;
        if (m.HasVal)
        {
            result = m.Val;
            return true;
        }
        if (m.Left != null && m.Right != null)
        {
            if (TrySolve(m.Left, out long lv) && TrySolve(m.Right, out long rv))
            {
                switch (m.Op)
                {
                    case '+': result = lv + rv; return true;
                    case '-': result = lv - rv; return true;
                    case '*': result = lv * rv; return true;
                    case '/':
                        if (rv == 0) return false;
                        result = lv / rv; return true;
                    case '=': result = (lv == rv) ? 1 : 0; return true;
                }
            }
        }
        return false;
    }

    static long Expect(Monkey m, long target)
    {
        if (m.Name == "humn") return target;

        bool hasLeft = m.Left != null;
        bool hasRight = m.Right != null;
        long lVal = 0, rVal = 0;
        bool lOk = hasLeft && TrySolve(m.Left, out lVal);
        bool rOk = hasRight && TrySolve(m.Right, out rVal);

        if (!lOk && hasLeft)
        {
            switch (m.Op)
            {
                case '+': return Expect(m.Left, target - rVal);
                case '-': return Expect(m.Left, target + rVal);
                case '*': return Expect(m.Left, target / rVal);
                case '/': return Expect(m.Left, target * rVal);
                case '=': return Expect(m.Left, rVal);
            }
        }

        if (!rOk && hasRight)
        {
            switch (m.Op)
            {
                case '+': return Expect(m.Right, target - lVal);
                case '-': return Expect(m.Right, lVal - target);
                case '*': return Expect(m.Right, target / lVal);
                case '/': return Expect(m.Right, lVal / target);
                case '=': return Expect(m.Right, lVal);
            }
        }

        throw new Exception("Cannot determine path");
    }

    static void Main(string[] args)
    {
        map.Clear();

        var path = "input.txt";
        if (!File.Exists(path))
        {
            Console.Error.WriteLine("Error opening input.txt");
            return;
        }

        foreach (var line in File.ReadLines(path))
        {
            var s = line.Trim();
            if (string.IsNullOrEmpty(s)) continue;

            var parts = s.Split(':');
            var name = parts[0].Trim();
            var rest = parts[1].Trim();

            if (long.TryParse(rest, out long val))
            {
                var m = FindOrCreate(name);
                m.Val = val;
                m.HasVal = true;
            }
            else
            {
                var tokens = rest.Split(' ', StringSplitOptions.RemoveEmptyEntries);
                var leftName = tokens[0];
                var op = tokens[1][0];
                var rightName = tokens[2];
                var m = FindOrCreate(name);
                m.Left = FindOrCreate(leftName);
                m.Right = FindOrCreate(rightName);
                m.Op = op;
                m.HasVal = false;
            }
        }

        var root = FindOrCreate("root");
        var humn = FindOrCreate("humn");

        root.Op = '=';
        humn.HasVal = false;

        var ans = Expect(root, 0);
        Console.WriteLine(ans);
    }
}
