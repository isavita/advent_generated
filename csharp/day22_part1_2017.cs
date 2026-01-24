using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var infected = new HashSet<(int x, int y)>();
        int height = lines.Length, width = lines[0].Length;
        for (int y = 0; y < height; y++)
            for (int x = 0; x < width; x++)
                if (lines[y][x] == '#')
                    infected.Add((x, y));

        int xPos = width / 2, yPos = height / 2;
        int dir = 0;
        int[] dx = { 0, 1, 0, -1 };
        int[] dy = { -1, 0, 1, 0 };
        int newInfections = 0;

        for (int i = 0; i < 10000; i++)
        {
            var pos = (xPos, yPos);
            if (infected.Contains(pos))
            {
                dir = (dir + 1) & 3;
                infected.Remove(pos);
            }
            else
            {
                dir = (dir + 3) & 3;
                infected.Add(pos);
                newInfections++;
            }
            xPos += dx[dir];
            yPos += dy[dir];
        }

        Console.WriteLine(newInfections);
    }
}