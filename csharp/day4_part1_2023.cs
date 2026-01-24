
using System;
using System.IO;
class Program
{
    static void Main()
    {
        int total = 0;
        foreach (var l in File.ReadLines("input.txt"))
        {
            var p = l.Split(':')[1].Split('|');
            var w = p[0].Split(' ', StringSplitOptions.RemoveEmptyEntries);
            var y = p[1].Split(' ', StringSplitOptions.RemoveEmptyEntries);
            int m = 0;
            foreach (var n in y)
                if (Array.IndexOf(w, n) >= 0) m = m == 0 ? 1 : m * 2;
            total += m;
        }
        Console.WriteLine(total);
    }
}
