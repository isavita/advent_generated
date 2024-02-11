
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static readonly (int, int)[] Neighbors4 = { (0, 1), (0, -1), (1, 0), (-1, 0) };

    static void Main()
    {
        var grid = new Dictionary<(int, int), int>();
        var visible = new HashSet<(int, int)>();
        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            int y = 0;
            while ((line = sr.ReadLine()) != null)
            {
                for (int x = 0; x < line.Length; x++)
                {
                    grid[(x, y)] = line[x] - '0';
                }
                y++;
            }
        }

        foreach (var p in grid.Keys)
        {
            foreach (var n in Neighbors4)
            {
                var next = p;
                while (true)
                {
                    next = (next.Item1 + n.Item1, next.Item2 + n.Item2);
                    if (grid.ContainsKey(next))
                    {
                        if (grid[next] >= grid[p])
                        {
                            break;
                        }
                    }
                    else
                    {
                        visible.Add(p);
                        break;
                    }
                }
            }
        }

        Console.WriteLine(visible.Count);
    }
}
