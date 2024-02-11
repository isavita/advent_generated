
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string initialState = "";
        Dictionary<string, char> rules = new Dictionary<string, char>();

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                if (line.Contains("initial state"))
                {
                    initialState = line.Split(": ")[1];
                }
                else if (line.Contains("=>"))
                {
                    string[] parts = line.Split(" => ");
                    rules[parts[0]] = parts[1][0];
                }
            }
        }

        Dictionary<int, char> state = new Dictionary<int, char>();
        for (int i = 0; i < initialState.Length; i++)
        {
            if (initialState[i] == '#')
            {
                state[i] = '#';
            }
        }

        for (int generation = 0; generation < 20; generation++)
        {
            Dictionary<int, char> newState = new Dictionary<int, char>();
            (int minPot, int maxPot) = MinMaxKeys(state);
            for (int i = minPot - 2; i <= maxPot + 2; i++)
            {
                string pattern = "";
                for (int j = i - 2; j <= i + 2; j++)
                {
                    if (state.ContainsKey(j) && state[j] == '#')
                    {
                        pattern += "#";
                    }
                    else
                    {
                        pattern += ".";
                    }
                }
                if (rules.ContainsKey(pattern) && rules[pattern] == '#')
                {
                    newState[i] = '#';
                }
            }
            state = newState;
        }

        int sum = 0;
        foreach (int key in state.Keys)
        {
            sum += key;
        }

        Console.WriteLine(sum);
    }

    static (int, int) MinMaxKeys(Dictionary<int, char> m)
    {
        int minKey = 0;
        int maxKey = 0;
        bool first = true;
        foreach (int k in m.Keys)
        {
            if (first)
            {
                minKey = k;
                maxKey = k;
                first = false;
            }
            else
            {
                if (k < minKey)
                {
                    minKey = k;
                }
                if (k > maxKey)
                {
                    maxKey = k;
                }
            }
        }
        return (minKey, maxKey);
    }
}
