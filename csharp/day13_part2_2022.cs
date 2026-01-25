
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public enum ItemType { Int, List }

public class Item
{
    public ItemType Type { get; }
    public int IntValue { get; }
    public List<Item> ListValue { get; }

    public Item(int value)
    {
        Type = ItemType.Int;
        IntValue = value;
    }

    public Item(List<Item> list)
    {
        Type = ItemType.List;
        ListValue = list;
    }
}

public class Program
{
    public static void Main(string[] args)
    {
        var packets = File.ReadAllLines("input.txt")
            .Where(line => !string.IsNullOrWhiteSpace(line))
            .Select(ParsePacket)
            .ToList();

        var divider1 = ParsePacket("[[2]]");
        var divider2 = ParsePacket("[[6]]");

        packets.Add(divider1);
        packets.Add(divider2);

        packets.Sort((a, b) => CompareItems(a, b));

        var divider1Pos = packets.IndexOf(divider1) + 1;
        var divider2Pos = packets.IndexOf(divider2) + 1;

        Console.WriteLine(divider1Pos * divider2Pos);
    }

    public static Item ParsePacket(string s)
    {
        var stack = new Stack<List<Item>>();
        stack.Push(new List<Item>());

        var i = 0;
        while (i < s.Length)
        {
            if (s[i] == '[')
            {
                stack.Push(new List<Item>());
                i++;
            }
            else if (s[i] == ']')
            {
                var list = stack.Pop();
                stack.Peek().Add(new Item(list));
                i++;
            }
            else if (s[i] == ',')
            {
                i++;
            }
            else
            {
                var start = i;
                while (i < s.Length && char.IsDigit(s[i])) i++;
                var num = int.Parse(s.Substring(start, i - start));
                stack.Peek().Add(new Item(num));
            }
        }

        return new Item(stack.Pop());
    }

    public static int CompareItems(Item a, Item b)
    {
        if (a.Type == ItemType.Int && b.Type == ItemType.Int)
        {
            return a.IntValue.CompareTo(b.IntValue);
        }

        if (a.Type == ItemType.List && b.Type == ItemType.List)
        {
            for (var i = 0; i < Math.Min(a.ListValue.Count, b.ListValue.Count); i++)
            {
                var cmp = CompareItems(a.ListValue[i], b.ListValue[i]);
                if (cmp != 0) return cmp;
            }
            return a.ListValue.Count.CompareTo(b.ListValue.Count);
        }

        if (a.Type == ItemType.Int)
        {
            return CompareItems(new Item(new List<Item> { a }), b);
        }

        return CompareItems(a, new Item(new List<Item> { b }));
    }
}
