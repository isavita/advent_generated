
using System;
using System.IO;

class Program
{
    static int Abs(int x)
    {
        return x < 0 ? -x : x;
    }

    static int Max(int a, int b)
    {
        return a > b ? a : b;
    }

    static int Distance(int x, int y, int z)
    {
        return (Abs(x) + Abs(y) + Abs(z)) / 2;
    }

    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        string[] directions = input.Split(',');

        int x = 0, y = 0, z = 0;
        int maxDistance = 0;

        foreach (string dir in directions)
        {
            switch (dir)
            {
                case "n":
                    y++;
                    z--;
                    break;
                case "ne":
                    x++;
                    z--;
                    break;
                case "se":
                    x++;
                    y--;
                    break;
                case "s":
                    y--;
                    z++;
                    break;
                case "sw":
                    x--;
                    z++;
                    break;
                case "nw":
                    x--;
                    y++;
                    break;
            }

            int curDistance = Distance(x, y, z);
            maxDistance = Max(maxDistance, curDistance);
        }

        Console.WriteLine(maxDistance);
    }
}
