
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        List<string> lines = new List<string>(File.ReadAllLines("input.txt"));
        List<List<bool>> asteroids = ReadAsteroids(lines);
        int maxCount = FindBestAsteroidLocation(asteroids);
        Console.WriteLine(maxCount);
    }

    static List<List<bool>> ReadAsteroids(List<string> lines)
    {
        List<List<bool>> asteroids = new List<List<bool>>();
        foreach (string line in lines)
        {
            List<bool> asteroidRow = new List<bool>();
            foreach (char c in line)
            {
                asteroidRow.Add(c == '#');
            }
            asteroids.Add(asteroidRow);
        }
        return asteroids;
    }

    static int FindBestAsteroidLocation(List<List<bool>> asteroids)
    {
        int maxCount = 0;
        for (int y = 0; y < asteroids.Count; y++)
        {
            for (int x = 0; x < asteroids[y].Count; x++)
            {
                if (asteroids[y][x])
                {
                    int count = CountVisibleAsteroids(asteroids, x, y);
                    if (count > maxCount)
                    {
                        maxCount = count;
                    }
                }
            }
        }
        return maxCount;
    }

    static int CountVisibleAsteroids(List<List<bool>> asteroids, int x, int y)
    {
        HashSet<double> angles = new HashSet<double>();
        for (int otherY = 0; otherY < asteroids.Count; otherY++)
        {
            for (int otherX = 0; otherX < asteroids[otherY].Count; otherX++)
            {
                if (asteroids[otherY][otherX] && !(otherX == x && otherY == y))
                {
                    double angle = Math.Atan2(otherY - y, otherX - x);
                    angles.Add(angle);
                }
            }
        }
        return angles.Count;
    }
}
