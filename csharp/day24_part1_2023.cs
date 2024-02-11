
using System;
using System.IO;
using System.Linq;

class Coord
{
    public double x;
    public double y;
    public double z;
}

class Point
{
    public Coord pos;
    public Coord vel;
}

class Program
{
    static Point[] ParseInput(string[] input)
    {
        return input.Select(line =>
        {
            var parts = line.Split('@');
            var posParts = parts[0].Split(',');
            var velParts = parts[1].Split(',');

            return new Point
            {
                pos = new Coord
                {
                    x = double.Parse(posParts[0]),
                    y = double.Parse(posParts[1]),
                    z = double.Parse(posParts[2])
                },
                vel = new Coord
                {
                    x = double.Parse(velParts[0]),
                    y = double.Parse(velParts[1]),
                    z = double.Parse(velParts[2])
                }
            };
        }).ToArray();
    }

    static bool IsIntersecting2D(Point p1, Point p2, out Coord coord, out double time1, out double time2)
    {
        double det = p1.vel.x * p2.vel.y - p2.vel.x * p1.vel.y;
        if (det == 0)
        {
            coord = new Coord();
            time1 = 0;
            time2 = 0;
            return false;
        }

        time1 = (p2.vel.y * (p2.pos.x - p1.pos.x) - p2.vel.x * (p2.pos.y - p1.pos.y)) / det;
        time2 = (p1.vel.y * (p2.pos.x - p1.pos.x) - p1.vel.x * (p2.pos.y - p1.pos.y)) / det;

        coord = new Coord
        {
            x = p1.pos.x + p1.vel.x * time1,
            y = p1.pos.y + p1.vel.y * time1,
            z = 0
        };

        return true;
    }

    static int Solve(string[] input, double min, double max)
    {
        var points = ParseInput(input);
        int cnt = 0;

        for (int i = 0; i < points.Length; i++)
        {
            for (int j = 0; j < i; j++)
            {
                if (IsIntersecting2D(points[i], points[j], out Coord coord, out double time1, out double time2))
                {
                    bool isInBound = min <= coord.x && coord.x <= max && min <= coord.y && coord.y <= max;
                    if (isInBound && time1 >= 0 && time2 >= 0)
                    {
                        cnt++;
                    }
                }
            }
        }

        return cnt;
    }

    static string[] ReadFile(string fileName)
    {
        return File.ReadAllLines(fileName);
    }

    static void Main()
    {
        string[] input = ReadFile("input.txt");
        Console.WriteLine(Solve(input, 200000000000000, 400000000000000));
    }
}
