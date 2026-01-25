
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    const int WIDTH = 101;
    const int HEIGHT = 103;
    const int STEPS = 100;
    const int MAX_ROBOTS = 1000;

    struct Robot
    {
        public int X, Y, Vx, Vy;
    }

    static void Main()
    {
        var robots = new List<Robot>(MAX_ROBOTS);
        foreach (var line in File.ReadLines("input.txt"))
        {
            if (robots.Count >= MAX_ROBOTS) break;
            var parts = line.Split(' ');
            var p = parts[0].Substring(2).Split(',');
            var v = parts[1].Substring(2).Split(',');
            robots.Add(new Robot
            {
                X = int.Parse(p[0]),
                Y = int.Parse(p[1]),
                Vx = int.Parse(v[0]),
                Vy = int.Parse(v[1])
            });
        }

        long q1 = 0, q2 = 0, q3 = 0, q4 = 0;
        foreach (var r in robots)
        {
            int x = Mod(r.X + STEPS * r.Vx, WIDTH);
            int y = Mod(r.Y + STEPS * r.Vy, HEIGHT);

            if (x == 50 || y == 51) continue;
            if (x < 50 && y < 51) q1++;
            else if (x > 50 && y < 51) q2++;
            else if (x < 50 && y > 51) q3++;
            else if (x > 50 && y > 51) q4++;
        }

        Console.WriteLine(q1 * q2 * q3 * q4);
    }

    static int Mod(int a, int m)
    {
        int r = a % m;
        return r < 0 ? r + m : r;
    }
}
