
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int totalRibbon = 0;

        foreach (string line in lines)
        {
            int[] dimensions = line.Split('x').Select(int.Parse).ToArray();
            if (dimensions.Length != 3)
            {
                throw new FormatException("Invalid input format");
            }

            int l = dimensions[0];
            int w = dimensions[1];
            int h = dimensions[2];

            // Calculate ribbon for the bow
            int bow = l * w * h;

            // Calculate ribbon for wrapping (smallest perimeter)
            int[] sides = { l, w, h };
            Array.Sort(sides);
            int wrap = 2 * sides[0] + 2 * sides[1];

            totalRibbon += bow + wrap;
        }

        Console.WriteLine(totalRibbon);
    }
}
