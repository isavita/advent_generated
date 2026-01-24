
using System;
using System.IO;
class Program
{
    static int calc(string s)
    {
        int len = 0, hex = 0;
        for (int i = 1; i < s.Length - 1; i++)
        {
            if (hex > 0) { hex--; continue; }
            if (s[i] == '\\')
            {
                if (i + 1 < s.Length - 1)
                {
                    i++;
                    if (s[i] == 'x') hex = 2;
                    len++;
                }
            }
            else len++;
        }
        return len;
    }

    static void Main()
    {
        int diff = 0;
        foreach (var s in File.ReadAllLines("input.txt"))
            diff += s.Length - calc(s);
        Console.WriteLine(diff);
    }
}
