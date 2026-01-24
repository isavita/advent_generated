
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    class SnailNumber
    {
        public int Value;
        public SnailNumber Left;
        public SnailNumber Right;
        public bool IsRegular => Left == null;
    }

    static SnailNumber Parse(string s, ref int i)
    {
        if (s[i] == '[')
        {
            i++;
            var left = Parse(s, ref i);
            i++; // ','
            var right = Parse(s, ref i);
            i++; // ']'
            return new SnailNumber { Left = left, Right = right, Value = -1 };
        }
        else
        {
            int v = 0;
            while (i < s.Length && char.IsDigit(s[i]))
                v = v * 10 + (s[i++] - '0');
            return new SnailNumber { Value = v };
        }
    }

    static bool Explode(SnailNumber n, int d, out int addLeft, out int addRight)
    {
        addLeft = addRight = 0;
        if (n.IsRegular) return false;
        if (d == 4)
        {
            addLeft = n.Left.Value;
            addRight = n.Right.Value;
            n.Left = n.Right = null;
            n.Value = 0;
            return true;
        }
        if (Explode(n.Left, d + 1, out var al, out var ar))
        {
            if (ar != 0) AddLeft(n.Right, ar);
            addLeft = al;
            return true;
        }
        if (Explode(n.Right, d + 1, out al, out ar))
        {
            if (al != 0) AddRight(n.Left, al);
            addRight = ar;
            return true;
        }
        return false;
    }

    static void AddLeft(SnailNumber n, int v)
    {
        if (n.IsRegular) n.Value += v;
        else AddLeft(n.Left, v);
    }

    static void AddRight(SnailNumber n, int v)
    {
        if (n.IsRegular) n.Value += v;
        else AddRight(n.Right, v);
    }

    static bool Split(SnailNumber n)
    {
        if (n.IsRegular)
        {
            if (n.Value < 10) return false;
            int l = n.Value / 2, r = (n.Value + 1) / 2;
            n.Left = new SnailNumber { Value = l };
            n.Right = new SnailNumber { Value = r };
            n.Value = -1;
            return true;
        }
        return Split(n.Left) || Split(n.Right);
    }

    static void Reduce(SnailNumber n)
    {
        while (true)
        {
            if (Explode(n, 0, out _, out _)) continue;
            if (Split(n)) continue;
            break;
        }
    }

    static SnailNumber Add(SnailNumber a, SnailNumber b)
    {
        var n = new SnailNumber { Left = a, Right = b, Value = -1 };
        Reduce(n);
        return n;
    }

    static long Magnitude(SnailNumber n)
    {
        if (n.IsRegular) return n.Value;
        return 3 * Magnitude(n.Left) + 2 * Magnitude(n.Right);
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var nums = new List<SnailNumber>();
        foreach (var l in lines)
        {
            int i = 0;
            nums.Add(Parse(l, ref i));
        }
        var res = nums[0];
        for (int i = 1; i < nums.Count; i++)
            res = Add(res, nums[i]);
        Console.WriteLine(Magnitude(res));
    }
}
