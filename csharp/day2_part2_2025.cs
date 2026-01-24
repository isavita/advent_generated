
using System;
using System.IO;

class Program
{
    static bool IsInvalid(ulong x)
    {
        string s = x.ToString();
        int n = s.Length;
        if (n <= 1) return false;

        for (int p = 1; p <= n / 2; p++)
        {
            if (n % p != 0) continue;
            int k = n / p;
            if (k < 2) continue;

            bool ok = true;
            for (int i = p; i < n && ok; i++)
                if (s[i] != s[i % p]) ok = false;

            if (ok) return true;
        }
        return false;
    }

    static void Main()
    {
        string txt = File.ReadAllText("input.txt");
        ulong sum = 0;
        int i = 0, len = txt.Length;

        while (i < len)
        {
            while (i < len && " \n\r\t,".IndexOf(txt[i]) >= 0) i++;
            if (i >= len) break;

            int j = i;
            while (j < len && char.IsDigit(txt[j])) j++;
            ulong a = ulong.Parse(txt.Substring(i, j - i));

            if (j >= len || txt[j] != '-') break;
            j++;

            i = j;
            while (j < len && char.IsDigit(txt[j])) j++;
            ulong b = ulong.Parse(txt.Substring(i, j - i));

            if (a > b) { var t = a; a = b; b = t; }

            for (ulong x = a; x <= b; x++)
            {
                if (IsInvalid(x)) sum += x;
                if (x == ulong.MaxValue) break;
            }

            i = j;
        }

        Console.WriteLine(sum);
    }
}
