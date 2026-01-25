
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var input = File.ReadAllText("input.txt");
        var tokens = input.Split((char[])null, StringSplitOptions.RemoveEmptyEntries);
        var stones = new List<string>(tokens);

        for (int i = 0; i < 25; i++)
        {
            var next = new List<string>(stones.Count * 2);
            foreach (var s in stones)
            {
                if (s == "0")
                {
                    next.Add("1");
                }
                else if (s.Length % 2 == 0)
                {
                    int mid = s.Length / 2;
                    string left = s.Substring(0, mid).TrimStart('0');
                    string right = s.Substring(mid).TrimStart('0');
                    if (left == "") left = "0";
                    if (right == "") right = "0";
                    next.Add(left);
                    next.Add(right);
                }
                else
                {
                    long n = long.Parse(s);
                    long res = n * 2024;
                    next.Add(res.ToString());
                }
            }
            stones = next;
        }

        Console.WriteLine(stones.Count);
    }
}
