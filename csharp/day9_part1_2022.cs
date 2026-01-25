
using System;
using System.Collections.Generic;
using System.IO;

public class Program
{
    public static void Main()
    {
        if (!File.Exists("input.txt"))
        {
            Console.WriteLine("File could not be opened.");
            return;
        }

        var lines = File.ReadAllLines("input.txt");
        var visited = new HashSet<(int, int)>();
        var head = (x: 0, y: 0);
        var tail = (x: 0, y: 0);

        visited.Add(tail);

        foreach (var line in lines)
        {
            var parts = line.Split(' ');
            var direction = parts[0];
            var steps = int.Parse(parts[1]);

            for (int i = 0; i < steps; i++)
            {
                switch (direction)
                {
                    case "R": head.x++; break;
                    case "L": head.x--; break;
                    case "U": head.y++; break;
                    case "D": head.y--; break;
                }

                if (Math.Abs(head.x - tail.x) > 1 || Math.Abs(head.y - tail.y) > 1)
                {
                    tail = (tail.x + Math.Sign(head.x - tail.x), tail.y + Math.Sign(head.y - tail.y));
                }

                visited.Add(tail);
            }
        }

        Console.WriteLine(visited.Count);
    }
}
