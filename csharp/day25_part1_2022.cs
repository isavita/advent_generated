
using System;
using System.IO;
using System.Text;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        long sum = 0;
        foreach (var line in lines)
        {
            var trimmed = line.Trim();
            if (trimmed.Length == 0) continue;
            sum += FromSnafu(trimmed);
        }
        Console.WriteLine(ToSnafu(sum));
    }

    static long FromSnafu(string s)
    {
        long n = 0;
        foreach (char c in s)
        {
            n *= 5;
            if (c == '=') n -= 2;
            else if (c == '-') n--;
            else n += c - '0';
        }
        return n;
    }

    static string ToSnafu(long n)
    {
        if (n == 0) return "0";
        var sb = new StringBuilder();
        while (n > 0)
        {
            int r = (int)(n % 5);
            if (r == 3)
            {
                n += 5;
                sb.Append('=');
            }
            else if (r == 4)
            {
                n += 5;
                sb.Append('-');
            }
            else
            {
                sb.Append((char)('0' + r));
            }
            n /= 5;
        }
        var res = new char[sb.Length];
        for (int i = 0; i < sb.Length; i++) res[i] = sb[sb.Length - 1 - i];
        return new string(res);
    }
}
