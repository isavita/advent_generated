
using System;
using System.IO;
using System.Linq;

public class Program
{
    public static void Main()
    {
        var asteroids = ReadAsteroids("input.txt");
        var bestLocation = FindBestAsteroidLocation(asteroids);
        var vaporized = VaporizeAsteroids(asteroids, bestLocation.X, bestLocation.Y);
        if (vaporized.Length >= 200)
        {
            var result = vaporized[199].X * 100 + vaporized[199].Y;
            Console.WriteLine(result);
        }
        else
        {
            Console.WriteLine("Less than 200 asteroids were vaporized.");
        }
    }

    private static bool[,] ReadAsteroids(string filename)
    {
        var lines = File.ReadAllLines(filename);
        var rows = lines.Length;
        var cols = lines[0].Length;
        var asteroids = new bool[rows, cols];

        for (var y = 0; y < rows; y++)
        {
            for (var x = 0; x < cols; x++)
            {
                asteroids[y, x] = lines[y][x] == '#';
            }
        }

        return asteroids;
    }

    private static (int X, int Y) FindBestAsteroidLocation(bool[,] asteroids)
    {
        var rows = asteroids.GetLength(0);
        var cols = asteroids.GetLength(1);
        var maxCount = 0;
        var bestX = 0;
        var bestY = 0;

        for (var y = 0; y < rows; y++)
        {
            for (var x = 0; x < cols; x++)
            {
                if (asteroids[y, x])
                {
                    var count = 0;
                    var angles = new System.Collections.Generic.HashSet<double>();

                    for (var otherY = 0; otherY < rows; otherY++)
                    {
                        for (var otherX = 0; otherX < cols; otherX++)
                        {
                            if (asteroids[otherY, otherX] && !(otherX == x && otherY == y))
                            {
                                var angle = Math.Atan2(otherY - y, otherX - x);
                                if (angles.Add(angle)) count++;
                            }
                        }
                    }

                    if (count > maxCount)
                    {
                        maxCount = count;
                        bestX = x;
                        bestY = y;
                    }
                }
            }
        }

        return (bestX, bestY);
    }

    private static Asteroid[] VaporizeAsteroids(bool[,] asteroids, int stationX, int stationY)
    {
        var rows = asteroids.GetLength(0);
        var cols = asteroids.GetLength(1);
        var targets = new System.Collections.Generic.List<Asteroid>();

        for (var y = 0; y < rows; y++)
        {
            for (var x = 0; x < cols; x++)
            {
                if (asteroids[y, x] && !(x == stationX && y == stationY))
                {
                    var angle = Math.Atan2(y - stationY, x - stationX);
                    if (angle < -Math.PI / 2) angle += 2 * Math.PI;
                    var dist = Math.Sqrt(Math.Pow(x - stationX, 2) + Math.Pow(y - stationY, 2));
                    targets.Add(new Asteroid(x, y, angle, dist));
                }
            }
        }

        targets = targets.OrderBy(a => a.Angle).ThenBy(a => a.Dist).ToList();
        var vaporized = new System.Collections.Generic.List<Asteroid>();

        while (targets.Count > 0)
        {
            var lastAngle = double.MinValue;
            var i = 0;
            while (i < targets.Count)
            {
                if (targets[i].Angle != lastAngle)
                {
                    vaporized.Add(targets[i]);
                    lastAngle = targets[i].Angle;
                    targets.RemoveAt(i);
                }
                else
                {
                    i++;
                }
            }
        }

        return vaporized.ToArray();
    }
}

public class Asteroid
{
    public int X { get; }
    public int Y { get; }
    public double Angle { get; }
    public double Dist { get; }

    public Asteroid(int x, int y, double angle, double dist)
    {
        X = x;
        Y = y;
        Angle = angle;
        Dist = dist;
    }
}
