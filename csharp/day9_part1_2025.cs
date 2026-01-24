
using System;
using System.IO;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int n = lines.Length;
        int[] x = new int[n];
        int[] y = new int[n];
        for (int i = 0; i < n; i++)
        {
            var sp = lines[i].Split(',');
            x[i] = int.Parse(sp[0]);
            y[i] = int.Parse(sp[1]);
        }

        long best = 0;
        for (int i = 0; i < n; i++)
        {
            int x1 = x[i];
            int y1 = y[i];
            for (int j = i; j < n; j++)
            {
                long dx = Math.Abs(x1 - x[j]) + 1L;
                long dy = Math.Abs(y1 - y[j]) + 1L;
                best = Math.Max(best, dx * dy);
            }
        }
        Console.WriteLine(best);
    }
}
