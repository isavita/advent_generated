
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string s = File.ReadAllText("input.txt").Trim();
        Console.WriteLine(FirstNUnique(s, 4));
    }

    static int FirstNUnique(string s, int n)
    {
        for (int i = n; i < s.Length; i++)
        {
            var b = s.Substring(i - n, n).ToCharArray();
            if (b.Length == new string(b).Distinct().Count())
            {
                return i;
            }
        }
        return -1;
    }
}
