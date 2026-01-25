
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var (k, l, m) = ReadInput("input.txt");

        var constraints = new int[14, 2];
        var stack = new int[14];
        var stackPtr = 0;

        for (var i = 0; i < 14; i++)
        {
            if (l[i] == 1)
            {
                stack[stackPtr++] = i;
            }
            else if (l[i] == 26)
            {
                var pop = stack[--stackPtr];
                constraints[pop, 0] = i;
                constraints[pop, 1] = m[pop] + k[i];
            }
        }

        var min = new int[14];
        for (var i = 0; i < 14; i++)
        {
            if (constraints[i, 0] == 0 && constraints[i, 1] == 0) continue;
            var vmin = 1;
            while (vmin + constraints[i, 1] < 1) vmin++;
            min[i] = vmin;
            min[constraints[i, 0]] = vmin + constraints[i, 1];
        }

        var n = min.Aggregate(0L, (acc, x) => acc * 10 + x);
        Console.WriteLine(n);
    }

    static (int[] k, int[] l, int[] m) ReadInput(string filePath)
    {
        var lines = File.ReadAllLines(filePath);
        var k = new int[14];
        var l = new int[14];
        var m = new int[14];

        for (var i = 0; i < 14; i++)
        {
            l[i] = int.Parse(lines[i * 18 + 4].Split(' ').Last());
            k[i] = int.Parse(lines[i * 18 + 5].Split(' ').Last());
            m[i] = int.Parse(lines[i * 18 + 15].Split(' ').Last());
        }

        return (k, l, m);
    }
}
