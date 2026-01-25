
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        if (!File.Exists("input.txt"))
        {
            Console.WriteLine("Error opening file");
            return;
        }

        var lines = File.ReadAllLines("input.txt");
        var k = new int[14];
        var l = new int[14];
        var m = new int[14];

        for (var i = 0; i < lines.Length; i++)
        {
            var line = lines[i];
            switch (i % 18)
            {
                case 4:
                    l[i / 18] = int.Parse(line.Split(' ').Last());
                    break;
                case 5:
                    k[i / 18] = int.Parse(line.Split(' ').Last());
                    break;
                case 15:
                    m[i / 18] = int.Parse(line.Split(' ').Last());
                    break;
            }
        }

        var constraints = new int[14, 2];
        var stack = new int[14];
        var stackSize = 0;
        for (var i = 0; i < 14; i++)
        {
            if (l[i] == 1)
            {
                stack[stackSize++] = i;
            }
            else if (l[i] == 26)
            {
                var pop = stack[--stackSize];
                constraints[pop, 0] = i;
                constraints[pop, 1] = m[pop] + k[i];
            }
        }

        var max = new int[14];
        for (var i = 0; i < 14; i++)
        {
            if (constraints[i, 0] == 0 && constraints[i, 1] == 0) continue;

            var vmax = 9;
            while (vmax + constraints[i, 1] > 9)
            {
                vmax--;
            }
            max[i] = vmax;
            max[constraints[i, 0]] = vmax + constraints[i, 1];
        }

        var n = long.Parse(string.Join("", max));
        Console.WriteLine(n);
    }
}
