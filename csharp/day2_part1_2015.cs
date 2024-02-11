
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int total = 0;

        foreach (string line in lines)
        {
            string[] dimensions = line.Split('x');
            if (dimensions.Length != 3)
            {
                throw new Exception("Invalid input format");
            }

            int l = int.Parse(dimensions[0]);
            int w = int.Parse(dimensions[1]);
            int h = int.Parse(dimensions[2]);

            int side1 = l * w;
            int side2 = w * h;
            int side3 = h * l;

            int smallest = Math.Min(side1, Math.Min(side2, side3));
            total += 2 * side1 + 2 * side2 + 2 * side3 + smallest;
        }

        Console.WriteLine(total);
    }
}
