
using System;
using System.Collections.Generic;
using System.IO;

class SnailNumber
{
    public SnailNumber(int value)
    {
        Value = value;
        IsRegular = true;
    }

    public SnailNumber(SnailNumber left, SnailNumber right)
    {
        Left = left;
        Right = right;
        IsRegular = false;
    }

    public bool IsRegular;
    public int Value;
    public SnailNumber Left;
    public SnailNumber Right;
}

class Program
{
    static bool Explode(SnailNumber sn, int depth, out int leftValue, out int rightValue)
    {
        leftValue = rightValue = 0;
        if (sn.IsRegular) return false;

        if (depth == 4)
        {
            leftValue = sn.Left.Value;
            rightValue = sn.Right.Value;
            sn.IsRegular = true;
            sn.Value = 0;
            sn.Left = sn.Right = null;
            return true;
        }

        if (Explode(sn.Left, depth + 1, out int l, out int r))
        {
            if (r > 0) AddLeft(sn.Right, r);
            rightValue = 0;
            leftValue = l;
            return true;
        }

        if (Explode(sn.Right, depth + 1, out l, out r))
        {
            if (l > 0) AddRight(sn.Left, l);
            leftValue = 0;
            rightValue = r;
            return true;
        }

        return false;
    }

    static void AddLeft(SnailNumber sn, int value)
    {
        if (sn.IsRegular) sn.Value += value;
        else AddLeft(sn.Left, value);
    }

    static void AddRight(SnailNumber sn, int value)
    {
        if (sn.IsRegular) sn.Value += value;
        else AddRight(sn.Right, value);
    }

    static bool Split(SnailNumber sn)
    {
        if (sn.IsRegular)
        {
            if (sn.Value >= 10)
            {
                sn.IsRegular = false;
                sn.Left = new SnailNumber(sn.Value / 2);
                sn.Right = new SnailNumber((sn.Value + 1) / 2);
                sn.Value = -1;
                return true;
            }
            return false;
        }
        return Split(sn.Left) || Split(sn.Right);
    }

    static SnailNumber Reduce(SnailNumber sn)
    {
        while (true)
        {
            if (Explode(sn, 0, out _, out _)) continue;
            if (!Split(sn)) break;
        }
        return sn;
    }

    static SnailNumber Add(SnailNumber a, SnailNumber b) => Reduce(new SnailNumber(a, b));

    static SnailNumber DeepCopy(SnailNumber sn)
    {
        if (sn.IsRegular) return new SnailNumber(sn.Value);
        return new SnailNumber(DeepCopy(sn.Left), DeepCopy(sn.Right));
    }

    static SnailNumber Parse(string s)
    {
        var stack = new Stack<SnailNumber>();
        foreach (var c in s)
        {
            if (char.IsDigit(c)) stack.Push(new SnailNumber(c - '0'));
            else if (c == ']')
            {
                var right = stack.Pop();
                var left = stack.Pop();
                stack.Push(new SnailNumber(left, right));
            }
        }
        return stack.Pop();
    }

    static int Magnitude(SnailNumber sn)
    {
        if (sn.IsRegular) return sn.Value;
        return 3 * Magnitude(sn.Left) + 2 * Magnitude(sn.Right);
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var snails = new List<SnailNumber>();
        foreach (var line in lines) snails.Add(Parse(line));

        int max = 0;
        for (int i = 0; i < snails.Count; i++)
        {
            for (int j = 0; j < snails.Count; j++)
            {
                if (i == j) continue;
                int m1 = Magnitude(Add(DeepCopy(snails[i]), DeepCopy(snails[j])));
                int m2 = Magnitude(Add(DeepCopy(snails[j]), DeepCopy(snails[i])));
                max = Math.Max(max, Math.Max(m1, m2));
            }
        }
        Console.WriteLine(max);
    }
}
