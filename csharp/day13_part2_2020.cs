
using System;
using System.IO;
using System.Linq;

class ShuttleSearch
{
    static void Main()
    {
        if (!File.Exists("input.txt"))
        {
            Console.WriteLine("File not found");
            return;
        }

        var lines = File.ReadAllLines("input.txt");
        int timestamp = int.Parse(lines[0]);
        var buses = lines[1].Split(',');

        // Part 1
        var busIds = buses.Where(b => b != "x").Select(int.Parse).ToArray();
        var minWait = busIds.Min(id => id - timestamp % id);
        var busId = busIds.First(id => id - timestamp % id == minWait);
        Console.WriteLine($"Part 1: {busId * minWait}");

        // Part 2
        long timestamp2 = 0;
        long step = 1;
        for (int i = 0; i < buses.Length; i++)
        {
            if (buses[i] != "x")
            {
                int id = int.Parse(buses[i]);
                while ((timestamp2 + i) % id != 0)
                {
                    timestamp2 += step;
                }
                step *= id;
            }
        }
        Console.WriteLine($"Part 2: {timestamp2}");
    }
}
