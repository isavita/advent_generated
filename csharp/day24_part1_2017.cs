
using System;
using System.IO;
using System.Linq;

class Program
{
    static int maxStrength;

    struct Component
    {
        public int a, b;
    }

    static void FindStrongestBridge(Component[] components, bool[] used, int port, int strength)
    {
        if (strength > maxStrength)
        {
            maxStrength = strength;
        }

        for (int i = 0; i < components.Length; i++)
        {
            if (used[i])
            {
                continue;
            }

            var c = components[i];
            if (c.a == port || c.b == port)
            {
                used[i] = true;
                int nextPort = c.a;
                if (c.a == port)
                {
                    nextPort = c.b;
                }
                FindStrongestBridge(components, used, nextPort, strength + c.a + c.b);
                used[i] = false;
            }
        }
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var components = lines.Select(line =>
        {
            var parts = line.Split('/');
            return new Component { a = int.Parse(parts[0]), b = int.Parse(parts[1]) };
        }).ToArray();

        bool[] used = new bool[components.Length];
        FindStrongestBridge(components, used, 0, 0);

        Console.WriteLine(maxStrength);
    }
}
