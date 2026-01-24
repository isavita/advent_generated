
using System;
using System.Collections.Generic;
using System.IO;

class Node
{
    public bool IsInt;
    public int IntValue;
    public List<Node> ListValue;
}

class Program
{
    static int Compare(Node a, Node b)
    {
        if (a.IsInt && b.IsInt)
            return a.IntValue.CompareTo(b.IntValue);

        if (a.IsInt)
        {
            var tmp = new Node { IsInt = false, ListValue = new List<Node> { a } };
            return Compare(tmp, b);
        }
        if (b.IsInt)
        {
            var tmp = new Node { IsInt = false, ListValue = new List<Node> { b } };
            return Compare(a, tmp);
        }

        for (int i = 0; i < Math.Min(a.ListValue.Count, b.ListValue.Count); i++)
        {
            int c = Compare(a.ListValue[i], b.ListValue[i]);
            if (c != 0) return c;
        }
        return a.ListValue.Count.CompareTo(b.ListValue.Count);
    }

    static Node Parse(string s, ref int i)
    {
        while (i < s.Length && char.IsWhiteSpace(s[i])) i++;
        if (s[i] == '[')
        {
            i++;
            var list = new List<Node>();
            while (s[i] != ']')
            {
                list.Add(Parse(s, ref i));
                if (s[i] == ',') i++;
            }
            i++;
            return new Node { IsInt = false, ListValue = list };
        }
        int num = 0;
        while (i < s.Length && char.IsDigit(s[i]))
        {
            num = num * 10 + (s[i] - '0');
            i++;
        }
        return new Node { IsInt = true, IntValue = num };
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int sum = 0, pair = 1;
        for (int i = 0; i < lines.Length; i += 3, pair++)
        {
            int p1 = 0, p2 = 0;
            var left = Parse(lines[i], ref p1);
            var right = Parse(lines[i + 1], ref p2);
            if (Compare(left, right) <= 0) sum += pair;
        }
        Console.WriteLine(sum);
    }
}
