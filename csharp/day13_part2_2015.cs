
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        Dictionary<string, Dictionary<string, int>> happinessMap = ReadHappinessValues("input.txt");
        AddYourself(ref happinessMap);

        List<string> guests = GetGuestList(happinessMap);
        int maxHappiness = CalculateOptimalArrangement(guests, happinessMap);
        Console.WriteLine(maxHappiness);
    }

    static Dictionary<string, Dictionary<string, int>> ReadHappinessValues(string filename)
    {
        Dictionary<string, Dictionary<string, int>> happinessMap = new Dictionary<string, Dictionary<string, int>>();
        string[] lines = File.ReadAllLines(filename);

        foreach (string line in lines)
        {
            string[] parts = line.Split(' ');
            if (parts.Length < 11) continue;

            string from = parts[0];
            string to = parts[10].Substring(0, parts[10].Length - 1);
            int change = int.Parse(parts[3]);
            if (parts[2] == "lose")
            {
                change = -change;
            }

            if (!happinessMap.ContainsKey(from))
            {
                happinessMap[from] = new Dictionary<string, int>();
            }
            happinessMap[from][to] = change;
        }

        return happinessMap;
    }

    static void AddYourself(ref Dictionary<string, Dictionary<string, int>> happinessMap)
    {
        happinessMap["You"] = new Dictionary<string, int>();
        foreach (string guest in happinessMap.Keys.ToList())
        {
            happinessMap[guest]["You"] = 0;
            happinessMap["You"][guest] = 0;
        }
    }

    static List<string> GetGuestList(Dictionary<string, Dictionary<string, int>> happinessMap)
    {
        return happinessMap.Keys.ToList();
    }

    static int CalculateOptimalArrangement(List<string> guests, Dictionary<string, Dictionary<string, int>> happinessMap)
    {
        int maxHappiness = 0;
        Permuate(guests, 0, ref maxHappiness, happinessMap);
        return maxHappiness;
    }

    static void Permuate(List<string> arr, int i, ref int maxHappiness, Dictionary<string, Dictionary<string, int>> happinessMap)
    {
        if (i > arr.Count)
        {
            return;
        }
        if (i == arr.Count)
        {
            int happiness = CalculateHappiness(arr, happinessMap);
            if (happiness > maxHappiness)
            {
                maxHappiness = happiness;
            }
            return;
        }
        for (int j = i; j < arr.Count; j++)
        {
            Swap(arr, i, j);
            Permuate(arr, i + 1, ref maxHappiness, happinessMap);
            Swap(arr, i, j);
        }
    }

    static void Swap(List<string> arr, int i, int j)
    {
        string temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }

    static int CalculateHappiness(List<string> arrangement, Dictionary<string, Dictionary<string, int>> happinessMap)
    {
        int happiness = 0;
        int n = arrangement.Count;
        for (int i = 0; i < n; i++)
        {
            int left = (i + n - 1) % n;
            int right = (i + 1) % n;
            happiness += happinessMap[arrangement[i]][arrangement[left]];
            happiness += happinessMap[arrangement[i]][arrangement[right]];
        }
        return happiness;
    }
}
