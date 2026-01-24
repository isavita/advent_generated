using System;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    static long ModPow(long b, long e, long m)
    {
        long r = 1;
        b %= m;
        while (e > 0)
        {
            if ((e & 1) == 1) r = (r * b) % m;
            b = (b * b) % m;
            e >>= 1;
        }
        return r;
    }

    static void Main()
    {
        var txt = File.ReadAllText("input.txt");
        var m = Regex.Match(txt, @"row (\d+), column (\d+)");
        long row = long.Parse(m.Groups[1].Value);
        long col = long.Parse(m.Groups[2].Value);
        long pos = (row + col - 2) * (row + col - 1) / 2 + col;
        const long start = 20151125;
        const long mul = 252533;
        const long mod = 33554393;
        long code = (start * ModPow(mul, pos - 1, mod)) % mod;
        Console.WriteLine(code);
    }
}
