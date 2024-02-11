
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        List<int> adapters = new List<int> { 0 };

        foreach (string line in lines)
        {
            adapters.Add(int.Parse(line));
        }

        adapters.Sort();
        adapters.Add(adapters.Last() + 3);

        Console.WriteLine(CountArrangements(adapters));
    }

    static long CountArrangements(List<int> adapters)
    {
        Dictionary<int, long> ways = new Dictionary<int, long>();
        ways[0] = 1;

        for (int i = 1; i < adapters.Count; i++)
        {
            int currentJoltage = adapters[i];
            ways[currentJoltage] = 0;

            foreach (int diff in new int[] { 1, 2, 3 })
            {
                ways[currentJoltage] += ways.GetValueOrDefault(currentJoltage - diff);
            }
        }

        return ways[adapters.Last()];
    }
}
