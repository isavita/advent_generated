
using System;
using System.IO;
using System.Linq;

class Claim
{
    public int ID { get; set; }
    public int X { get; set; }
    public int Y { get; set; }
    public int Width { get; set; }
    public int Height { get; set; }
}

class Program
{
    static void Main()
    {
        var claims = ReadClaims("input.txt");

        int[][] fabric = new int[1000][];
        for (int i = 0; i < 1000; i++)
        {
            fabric[i] = new int[1000];
        }

        foreach (var claim in claims)
        {
            for (int y = claim.Y; y < claim.Y + claim.Height; y++)
            {
                for (int x = claim.X; x < claim.X + claim.Width; x++)
                {
                    fabric[y][x]++;
                }
            }
        }

        foreach (var claim in claims)
        {
            bool overlap = false;
            for (int y = claim.Y; y < claim.Y + claim.Height; y++)
            {
                for (int x = claim.X; x < claim.X + claim.Width; x++)
                {
                    if (fabric[y][x] > 1)
                    {
                        overlap = true;
                        break;
                    }
                }
                if (overlap)
                {
                    break;
                }
            }
            if (!overlap)
            {
                Console.WriteLine(claim.ID);
                return;
            }
        }
    }

    static Claim[] ReadClaims(string filename)
    {
        string[] lines = File.ReadAllLines(filename);
        return lines.Select(line =>
        {
            string[] parts = line.Split(' ');
            int id = int.Parse(parts[0].Substring(1));
            string[] coords = parts[2].Substring(0, parts[2].Length - 1).Split(',');
            int x = int.Parse(coords[0]);
            int y = int.Parse(coords[1]);
            string[] dims = parts[3].Split('x');
            int width = int.Parse(dims[0]);
            int height = int.Parse(dims[1]);
            return new Claim { ID = id, X = x, Y = y, Width = width, Height = height };
        }).ToArray();
    }
}
