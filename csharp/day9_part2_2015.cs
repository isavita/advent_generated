
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        Dictionary<string, Dictionary<string, int>> distances = ReadAndParseInput("input.txt");
        List<string> locations = GetUniqueLocations(distances);
        int maxDistance = FindLongestRoute(locations, distances);
        Console.WriteLine(maxDistance);
    }

    static Dictionary<string, Dictionary<string, int>> ReadAndParseInput(string filename)
    {
        var distances = new Dictionary<string, Dictionary<string, int>>();
        using (StreamReader sr = new StreamReader(filename))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                string[] parts = line.Split(' ');
                if (parts.Length != 5)
                    continue;

                string from = parts[0];
                string to = parts[2];
                int distance = int.Parse(parts[4]);

                if (!distances.ContainsKey(from))
                    distances[from] = new Dictionary<string, int>();
                distances[from][to] = distance;

                if (!distances.ContainsKey(to))
                    distances[to] = new Dictionary<string, int>();
                distances[to][from] = distance;
            }
        }
        return distances;
    }

    static List<string> GetUniqueLocations(Dictionary<string, Dictionary<string, int>> distances)
    {
        HashSet<string> locationSet = new HashSet<string>();
        foreach (var from in distances.Keys)
        {
            locationSet.Add(from);
            foreach (var to in distances[from].Keys)
            {
                locationSet.Add(to);
            }
        }
        return locationSet.ToList();
    }

    static int FindLongestRoute(List<string> locations, Dictionary<string, Dictionary<string, int>> distances)
    {
        int maxDistance = 0;
        Permuate(locations, 0, ref maxDistance, distances, false);
        return maxDistance;
    }

    static void Permuate(List<string> arr, int i, ref int bestDistance, Dictionary<string, Dictionary<string, int>> distances, bool findShortest)
    {
        if (i > arr.Count)
            return;
        if (i == arr.Count)
        {
            int dist = CalculateRouteDistance(arr, distances);
            if (findShortest)
            {
                if (bestDistance == 0 || dist < bestDistance)
                    bestDistance = dist;
            }
            else
            {
                if (dist > bestDistance)
                    bestDistance = dist;
            }
            return;
        }
        for (int j = i; j < arr.Count; j++)
        {
            Swap(ref arr[i], ref arr[j]);
            Permuate(arr, i + 1, ref bestDistance, distances, findShortest);
            Swap(ref arr[i], ref arr[j]);
        }
    }

    static void Swap(ref string a, ref string b)
    {
        string temp = a;
        a = b;
        b = temp;
    }

    static int CalculateRouteDistance(List<string> route, Dictionary<string, Dictionary<string, int>> distances)
    {
        int sum = 0;
        for (int i = 0; i < route.Count - 1; i++)
        {
            sum += distances[route[i]][route[i + 1]];
        }
        return sum;
    }
}
