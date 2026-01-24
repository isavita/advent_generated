using System;
using System.Collections.Generic;
using System.IO;

enum State { Clean, Weakened, Infected, Flagged }

class Program
{
    static long Key(int x, int y) => ((long)x << 32) ^ (uint)y;

    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var grid = new Dictionary<long, State>();
        int height = lines.Length, width = lines[0].Length;
        int startX = width / 2, startY = height / 2;

        for (int y = 0; y < height; y++)
            for (int x = 0; x < width; x++)
                if (lines[y][x] == '#')
                    grid[Key(x, y)] = State.Infected;

        int[] dx = { 0, 1, 0, -1 };
        int[] dy = { -1, 0, 1, 0 };
        int xPos = startX, yPos = startY, dir = 0, infected = 0;

        for (int i = 0; i < 10_000_000; i++)
        {
            var k = Key(xPos, yPos);
            grid.TryGetValue(k, out var s);
            switch (s)
            {
                case State.Clean:
                    dir = (dir + 3) & 3;
                    grid[k] = State.Weakened;
                    break;
                case State.Weakened:
                    grid[k] = State.Infected;
                    infected++;
                    break;
                case State.Infected:
                    dir = (dir + 1) & 3;
                    grid[k] = State.Flagged;
                    break;
                case State.Flagged:
                    dir = (dir + 2) & 3;
                    grid.Remove(k);
                    break;
            }
            xPos += dx[dir];
            yPos += dy[dir];
        }

        Console.WriteLine(infected);
    }
}
