
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

struct Robot
{
    public int X, Y, Vx, Vy;
}

class Program
{
    static int Mod(int a, int b) => (a % b + b) % b;

    static void MoveRobots(Robot[] robots, int sizeX, int sizeY)
    {
        for (int i = 0; i < robots.Length; i++)
        {
            robots[i].X = Mod(robots[i].X + robots[i].Vx, sizeX);
            robots[i].Y = Mod(robots[i].Y + robots[i].Vy, sizeY);
        }
    }

    static int CountQuadrants(Robot[] robots, int sizeX, int sizeY)
    {
        int[] counts = new int[4];
        int centerX = sizeX / 2;
        int centerY = sizeY / 2;
        foreach (var r in robots)
        {
            int x = r.X, y = r.Y;
            if (x < centerX)
            {
                if (y < centerY) counts[0]++;
                else if (y > centerY) counts[1]++;
            }
            else if (x > centerX)
            {
                if (y < centerY) counts[2]++;
                else if (y > centerY) counts[3]++;
            }
        }
        int product = 1;
        foreach (var c in counts) product *= c;
        return product;
    }

    static bool HasNoOverlaps(Robot[] robots)
    {
        var seen = new HashSet<(int,int)>();
        foreach (var r in robots)
            if (!seen.Add((r.X, r.Y))) return false;
        return true;
    }

    static void DrawGrid(Robot[] robots, int sizeX, int sizeY)
    {
        var grid = new char[sizeY, sizeX];
        for (int y = 0; y < sizeY; y++)
            for (int x = 0; x < sizeX; x++)
                grid[y, x] = '.';
        foreach (var r in robots)
            grid[r.Y, r.X] = '#';
        for (int y = 0; y < sizeY; y++)
        {
            for (int x = 0; x < sizeX; x++)
                Console.Write(grid[y, x]);
            Console.WriteLine();
        }
    }

    static void Main()
    {
        const int sizeX = 101;
        const int sizeY = 103;
        const int maxRobots = 1000;

        var lines = File.ReadAllLines("input.txt");
        var robots = new List<Robot>();
        var regex = new Regex(@"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)");
        foreach (var line in lines)
            if (!string.IsNullOrWhiteSpace(line))
            {
                var m = regex.Match(line);
                if (m.Success)
                    robots.Add(new Robot
                    {
                        X = int.Parse(m.Groups[1].Value),
                        Y = int.Parse(m.Groups[2].Value),
                        Vx = int.Parse(m.Groups[3].Value),
                        Vy = int.Parse(m.Groups[4].Value)
                    });
            }

        var robotsPart1 = new Robot[robots.Count];
        Array.Copy(robots.ToArray(), robotsPart1, robots.Count);
        for (int i = 0; i < 100; i++) MoveRobots(robotsPart1, sizeX, sizeY);
        int safetyFactor = CountQuadrants(robotsPart1, sizeX, sizeY);
        Console.WriteLine($"Part 1 - Safety Factor after 100 seconds: {safetyFactor}");

        var robotsPart2 = new Robot[robots.Count];
        Array.Copy(robots.ToArray(), robotsPart2, robots.Count);
        int seconds = 0;
        while (!HasNoOverlaps(robotsPart2))
        {
            MoveRobots(robotsPart2, sizeX, sizeY);
            seconds++;
            if (seconds > 1000000)
            {
                Console.WriteLine("Exceeded maximum iterations without finding a unique position configuration.");
                return;
            }
        }
        Console.WriteLine($"Part 2 - Fewest seconds to display Easter egg: {seconds}");
        Console.WriteLine("Final positions of robots:");
        DrawGrid(robotsPart2, sizeX, sizeY);
    }
}
