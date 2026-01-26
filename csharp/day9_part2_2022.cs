
using System;
using System.IO;
using System.Collections.Generic;

public class Program
{
    public static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        Console.WriteLine(Visited(input, 10));
    }

    public static int Visited(string input, int ropeLen)
    {
        int[][] rope = new int[ropeLen][];
        for (int i = 0; i < ropeLen; i++)
            rope[i] = new int[2];

        var visited = new HashSet<(int, int)>();
        foreach (string line in input.Split('\n'))
        {
            string[] parts = line.Split(' ');
            char dir = parts[0][0];
            int n = int.Parse(parts[1]);
            int dx = 0, dy = 0;
            switch (dir)
            {
                case 'N':
                case 'U':
                case '^':
                    dy = 1;
                    break;
                case 'S':
                case 'D':
                case 'v':
                    dy = -1;
                    break;
                case 'E':
                case 'R':
                case '>':
                    dx = 1;
                    break;
                case 'W':
                case 'L':
                case '<':
                    dx = -1;
                    break;
            }
            for (int i = 0; i < n; i++)
            {
                rope[0][0] += dx;
                rope[0][1] += dy;
                for (int j = 1; j < ropeLen; j++)
                {
                    rope[j] = Next(rope[j - 1], rope[j]);
                }
                visited.Add((rope[ropeLen - 1][0], rope[ropeLen - 1][1]));
            }
        }
        return visited.Count;
    }

    public static int[] Next(int[] head, int[] tail)
    {
        if (Math.Abs(head[0] - tail[0]) <= 1 && Math.Abs(head[1] - tail[1]) <= 1)
        {
            return tail;
        }
        int dx = Sign(head[0] - tail[0]);
        int dy = Sign(head[1] - tail[1]);
        return new int[] { tail[0] + dx, tail[1] + dy };
    }

    public static int Sign(int n)
    {
        if (n == 0)
        {
            return 0;
        }
        return n < 0 ? -1 : 1;
    }
}
