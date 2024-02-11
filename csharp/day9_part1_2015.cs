
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var distances = ReadAndParseInput("input.txt");
        var locations = GetUniqueLocations(distances);
        var minDistance = FindShortestRoute(locations, distances);
        Console.WriteLine(minDistance);
    }

    static Dictionary<string, Dictionary<string, int>> ReadAndParseInput(string filename)
    {
        var distances = new Dictionary<string, Dictionary<string, int>>();
        foreach (var line in File.ReadLines(filename))
        {
            var parts = line.Split(' ');
            if (parts.Length != 5) continue;

            var from = parts[0];
            var to = parts[2];
            var dist = int.Parse(parts[4]);

            if (!distances.ContainsKey(from))
                distances[from] = new Dictionary<string, int>();
            distances[from][to] = dist;

            if (!distances.ContainsKey(to))
                distances[to] = new Dictionary<string, int>();
            distances[to][from] = dist;
        }

        return distances;
    }

    static List<string> GetUniqueLocations(Dictionary<string, Dictionary<string, int>> distances)
    {
        var locationSet = new HashSet<string>();
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

    static int FindShortestRoute(List<string> locations, Dictionary<string, Dictionary<string, int>> distances)
    {
        int minDistance = -1;
        Permuate(locations, 0, ref minDistance, distances);
        return minDistance;
    }

    static void Permuate(List<string> arr, int i, ref int minDistance, Dictionary<string, Dictionary<string, int>> distances)
    {
        if (i > arr.Count) return;
        if (i == arr.Count)
        {
            var dist = CalculateRouteDistance(arr, distances);
            if (minDistance == -1 || dist < minDistance)
                minDistance = dist;
            return;
        }
        for (int j = i; j < arr.Count; j++)
        {
            Swap(arr, i, j);
            Permuate(arr, i + 1, ref minDistance, distances);
            Swap(arr, i, j);
        }
    }

    static void Swap(List<string> arr, int i, int j)
    {
        var temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
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
