
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static readonly List<(int, int)> Neighbors4 = new List<(int, int)> { (0, 1), (0, -1), (1, 0), (-1, 0) };

    static void Main()
    {
        var grid = new Dictionary<(int, int), int>();
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

        int maxScore = 0;
        foreach (var p in grid)
        {
            int score = 1;
            foreach (var n in Neighbors4)
            {
                var next = p.Key;
                int view = 0;
                while (true)
                {
                    next = (next.Item1 + n.Item1, next.Item2 + n.Item2);
                    if (grid.ContainsKey(next))
                    {
                        view++;
                        if (grid[next] >= grid[p.Key])
                        {
                            score *= view;
                            break;
                        }
                    }
                    else
                    {
                        score *= view;
                        break;
                    }
                }
            }

            if (score > maxScore)
            {
                maxScore = score;
            }
        }
        Console.WriteLine(maxScore);
    }
}
