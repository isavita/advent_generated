
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static int CountOrbits(Dictionary<string, List<string>> orbitMap, string start, int depth)
    {
        if (!orbitMap.ContainsKey(start))
        {
            return depth;
        }

        int count = depth;
        foreach (var orbit in orbitMap[start])
        {
            count += CountOrbits(orbitMap, orbit, depth + 1);
        }
        return count;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Dictionary<string, List<string>> orbitMap = new Dictionary<string, List<string>>();

        foreach (var line in lines)
        {
            string[] parts = line.Split(')');
            string center = parts[0];
            string orbiter = parts[1];
            if (!orbitMap.ContainsKey(center))
            {
                orbitMap[center] = new List<string>();
            }
            orbitMap[center].Add(orbiter);
        }

        int totalOrbits = CountOrbits(orbitMap, "COM", 0);
        Console.WriteLine(totalOrbits);
    }
}
