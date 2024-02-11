
using System;
using System.IO;
using System.Linq;

class Component
{
    public int a, b;

    public Component(int a, int b)
    {
        this.a = a;
        this.b = b;
    }
}

class Program
{
    static int maxStrength = 0;
    static int maxLength = 0;

    static void FindStrongestLongestBridge(Component[] components, bool[] used, int port, int strength, int length)
    {
        if (length > maxLength || (length == maxLength && strength > maxStrength))
        {
            maxStrength = strength;
            maxLength = length;
        }

        for (int i = 0; i < components.Length; i++)
        {
            if (used[i])
            {
                continue;
            }

            Component c = components[i];
            if (c.a == port || c.b == port)
            {
                used[i] = true;
                int nextPort = c.a;
                if (c.a == port)
                {
                    nextPort = c.b;
                }
                FindStrongestLongestBridge(components, used, nextPort, strength + c.a + c.b, length + 1);
                used[i] = false;
            }
        }
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Component[] components = lines.Select(line =>
        {
            string[] ports = line.Split('/');
            int a = int.Parse(ports[0]);
            int b = int.Parse(ports[1]);
            return new Component(a, b);
        }).ToArray();

        bool[] used = new bool[components.Length];
        FindStrongestLongestBridge(components, used, 0, 0, 0);

        Console.WriteLine(maxStrength);
    }
}
