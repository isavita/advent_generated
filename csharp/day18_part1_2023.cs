
using System;
using System.IO;
using System.Linq;

public class LavaductLagoon
{
    public static void Main(string[] args)
    {
        try
        {
            var lines = File.ReadAllLines("input.txt");
            var area = CalculateArea(lines);
            Console.WriteLine(area);
        }
        catch (FileNotFoundException)
        {
            Console.Error.WriteLine("Error reading input file: File not found");
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine("Error reading input file: " + ex.Message);
        }
    }

    private static long CalculateArea(string[] instructions)
    {
        var vertices = new (long x, long y)[] { (0, 0) }.ToList();
        long perimeter = 0;

        foreach (var instruction in instructions)
        {
            var parts = instruction.Split(' ');
            var direction = parts[0][0];
            var steps = int.Parse(parts[1]);
            perimeter += steps;

            var lastVertex = vertices.Last();
            var nextX = lastVertex.x;
            var nextY = lastVertex.y;

            switch (direction)
            {
                case 'U':
                    nextY -= steps;
                    break;
                case 'D':
                    nextY += steps;
                    break;
                case 'L':
                    nextX -= steps;
                    break;
                case 'R':
                    nextX += steps;
                    break;
            }
            vertices.Add((nextX, nextY));
        }

        // Shoelace formula to calculate area
        var area = 0L;
        for (var i = 0; i < vertices.Count - 1; i++)
        {
            area += (vertices[i].x * vertices[i + 1].y) - (vertices[i + 1].x * vertices[i].y);
        }
        area = Math.Abs(area) / 2;

        // Pick's theorem: A = I + B/2 - 1, where A is area, I is interior points, B is boundary points
        // We want to find I + B, which is the total volume
        // I + B = A + B/2 + 1
        return area + perimeter / 2 + 1;
    }
}
