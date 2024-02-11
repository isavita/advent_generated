
using System;
using System.IO;
using System.Linq;

class Program
{
    static (int, int) ParseRange(string r)
    {
        var parts = r.Split('-');
        return (int.Parse(parts[0]), int.Parse(parts[1]));
    }

    static void Main()
    {
        int count = 0;
        string[] lines = File.ReadAllLines("input.txt");

        foreach (var line in lines)
        {
            var ranges = line.Split(',');
            if (ranges.Length != 2)
            {
                continue;
            }

            var (start1, end1) = ParseRange(ranges[0]);
            var (start2, end2) = ParseRange(ranges[1]);

            if ((start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1))
            {
                count++;
            }
        }

        Console.WriteLine(count);
    }
}
