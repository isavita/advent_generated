
using System;
using System.IO;
using System.Linq;

class Program
{
    const int MAX_LINES = 200;
    const int MAX_WIDTH = 200;
    const long EXPANSION_FACTOR = 1000000;

    struct Coord
    {
        public int X;
        public int Y;
    }

    static void Main(string[] args)
    {
        var galaxies = new Coord[MAX_LINES * MAX_WIDTH];
        var rowHasGalaxy = new bool[MAX_LINES];
        var colHasGalaxy = new bool[MAX_WIDTH];
        int numGalaxies = 0;

        try
        {
            var lines = File.ReadAllLines("input.txt");
            if (lines.Length > MAX_LINES)
                throw new Exception($"Number of lines exceeds {MAX_LINES}");

            int width = lines[0].Length;
            if (width > MAX_WIDTH)
                throw new Exception($"Line width {width} exceeds {MAX_WIDTH}");

            for (int i = 0; i < lines.Length; i++)
            {
                if (lines[i].Length != width)
                    throw new Exception($"Inconsistent line width at line {i + 1}");

                for (int j = 0; j < width; j++)
                {
                    if (lines[i][j] == '#')
                    {
                        galaxies[numGalaxies] = new Coord { X = j, Y = i };
                        rowHasGalaxy[i] = true;
                        colHasGalaxy[j] = true;
                        numGalaxies++;
                    }
                }
            }

            long[] rowOffset = new long[MAX_LINES];
            long[] colOffset = new long[MAX_WIDTH];

            long currentOffset = 0;
            for (int i = 0; i < lines.Length; i++)
            {
                rowOffset[i] = currentOffset;
                if (!rowHasGalaxy[i]) currentOffset++;
            }

            currentOffset = 0;
            for (int i = 0; i < width; i++)
            {
                colOffset[i] = currentOffset;
                if (!colHasGalaxy[i]) currentOffset++;
            }

            long totalDistance = 0;
            long expansionAdd = EXPANSION_FACTOR - 1;

            for (int i = 0; i < numGalaxies; i++)
            {
                for (int j = i + 1; j < numGalaxies; j++)
                {
                    long x1 = galaxies[i].X + colOffset[galaxies[i].X] * expansionAdd;
                    long y1 = galaxies[i].Y + rowOffset[galaxies[i].Y] * expansionAdd;
                    long x2 = galaxies[j].X + colOffset[galaxies[j].X] * expansionAdd;
                    long y2 = galaxies[j].Y + rowOffset[galaxies[j].Y] * expansionAdd;

                    totalDistance += Math.Abs(x1 - x2) + Math.Abs(y1 - y2);
                }
            }

            Console.WriteLine(totalDistance);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine(ex.Message);
        }
    }
}
