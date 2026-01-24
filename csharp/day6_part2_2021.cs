
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var counts = new long[9];
        foreach (var s in File.ReadAllText("input.txt").Split(',', StringSplitOptions.RemoveEmptyEntries))
            counts[int.Parse(s)]++;

        for (int day = 0; day < 256; day++)
        {
            long spawn = counts[0];
            Array.Copy(counts, 1, counts, 0, 8);
            counts[6] += spawn;
            counts[8] = spawn;
        }

        Console.WriteLine(counts.Sum());
    }
}
