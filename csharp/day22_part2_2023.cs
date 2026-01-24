
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    struct Brick
    {
        public int X1, Y1, Z1;
        public int X2, Y2, Z2;
        public List<int> BasedOn;
        public List<int> Support;
    }

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        Brick[] bricks = new Brick[lines.Length];
        for (int i = 0; i < lines.Length; i++)
        {
            var parts = lines[i].Split('~');
            var c1 = parts[0].Split(',').Select(int.Parse).ToArray();
            var c2 = parts[1].Split(',').Select(int.Parse).ToArray();
            bricks[i] = new Brick
            {
                X1 = c1[0],
                Y1 = c1[1],
                Z1 = c1[2],
                X2 = c2[0],
                Y2 = c2[1],
                Z2 = c2[2],
                BasedOn = new List<int>(),
                Support = new List<int>()
            };
        }

        // sort by Z
        Array.Sort(bricks, (a, b) => a.Z2.CompareTo(b.Z2));

        // settle
        for (int i = 0; i < bricks.Length; i++)
        {
            int bestZ = 0;
            List<int> supports = new List<int>();
            for (int j = 0; j < i; j++)
            {
                if (Overlap(bricks[i], bricks[j]))
                {
                    if (bricks[j].Z2 == bestZ) supports.Add(j);
                    else if (bricks[j].Z2 > bestZ)
                    {
                        bestZ = bricks[j].Z2;
                        supports.Clear();
                        supports.Add(j);
                    }
                }
            }
            int delta = bricks[i].Z2 - bricks[i].Z1;
            bricks[i].Z1 = bestZ + 1;
            bricks[i].Z2 = bricks[i].Z1 + delta;
            foreach (int b in supports)
            {
                bricks[i].BasedOn.Add(b);
                bricks[b].Support.Add(i);
            }
        }

        int total = 0;
        for (int i = 0; i < bricks.Length; i++)
        {
            Queue<int> q = new Queue<int>();
            HashSet<int> falling = new HashSet<int>();
            foreach (int b in bricks[i].Support)
                if (bricks[b].BasedOn.Count == 1)
                    q.Enqueue(b);
            while (q.Count > 0)
            {
                int b = q.Dequeue();
                if (!falling.Add(b)) continue;
                foreach (int s in bricks[b].Support)
                {
                    if (bricks[s].BasedOn.All(falling.Contains))
                        q.Enqueue(s);
                }
            }
            total += falling.Count;
        }
        Console.WriteLine(total);
    }

    static bool Overlap(Brick a, Brick b)
    {
        return Math.Max(a.X1, b.X1) <= Math.Min(a.X2, b.X2) &&
               Math.Max(a.Y1, b.Y1) <= Math.Min(a.Y2, b.Y2);
    }
}
