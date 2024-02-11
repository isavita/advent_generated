
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Program
{
    static List<long> GenerateAddresses(string mask, long address)
    {
        List<int> floating = new List<int>();
        List<long> addresses = new List<long>();

        for (int i = 0; i < mask.Length; i++)
        {
            if (mask[i] == '1')
            {
                address |= (1L << (35 - i));
            }
            else if (mask[i] == 'X')
            {
                floating.Add(35 - i);
            }
        }

        int count = 1 << floating.Count;
        for (int i = 0; i < count; i++)
        {
            long modAddress = address;
            for (int j = 0; j < floating.Count; j++)
            {
                if ((i & (1 << j)) == 0)
                {
                    modAddress &= ~(1L << floating[j]);
                }
                else
                {
                    modAddress |= (1L << floating[j]);
                }
            }
            addresses.Add(modAddress);
        }

        return addresses;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        string mask = "";
        Dictionary<long, long> mem = new Dictionary<long, long>();
        Regex reMem = new Regex(@"mem\[(\d+)] = (\d+)");

        foreach (string line in lines)
        {
            if (line.StartsWith("mask = "))
            {
                mask = line.Substring(7);
            }
            else
            {
                Match match = reMem.Match(line);
                if (match.Success)
                {
                    long address = long.Parse(match.Groups[1].Value);
                    long value = long.Parse(match.Groups[2].Value);
                    List<long> addresses = GenerateAddresses(mask, address);
                    foreach (long addr in addresses)
                    {
                        mem[addr] = value;
                    }
                }
            }
        }

        long sum = 0;
        foreach (long value in mem.Values)
        {
            sum += value;
        }

        Console.WriteLine(sum);
    }
}
