
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Program
{
    static long ApplyMask(long value, string mask)
    {
        long result = 0;
        for (int i = 0; i < 36; i++)
        {
            long bitValue = 1L << (35 - i);
            if (mask[i] == '1')
            {
                result |= bitValue;
            }
            else if (mask[i] == 'X')
            {
                result |= (value & bitValue);
            }
        }
        return result;
    }

    static void Main()
    {
        string mask = "";
        Dictionary<long, long> mem = new Dictionary<long, long>();
        Regex reMem = new Regex(@"mem\[(\d+)] = (\d+)");

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
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
                        long address = Int64.Parse(match.Groups[1].Value);
                        long value = Int64.Parse(match.Groups[2].Value);
                        mem[address] = ApplyMask(value, mask);
                    }
                }
            }
        }

        long sum = 0;
        foreach (var value in mem.Values)
        {
            sum += value;
        }

        Console.WriteLine(sum);
    }
}
