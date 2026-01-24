
using System;
using System.IO;

class Program
{
    static int Calc(string s)
    {
        int len = s.Length;
        for (int d1 = 9; d1 >= 0; --d1)
        {
            int p = s.IndexOf((char)('0' + d1));
            if (p < 0 || p == len - 1) continue;
            int max2 = -1;
            for (int i = p + 1; i < len; ++i)
            {
                char c = s[i];
                if (char.IsDigit(c))
                {
                    int v = c - '0';
                    if (v > max2) max2 = v;
                    if (max2 == 9) break;
                }
            }
            if (max2 != -1) return d1 * 10 + max2;
        }
        return 0;
    }

    static void Main()
    {
        int total = 0;
        foreach (var line in File.ReadLines("input.txt"))
            total += Calc(line);
        Console.WriteLine(total);
    }
}
