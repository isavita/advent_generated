
using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        int twoCount = 0;
        int threeCount = 0;

        foreach (string line in lines)
        {
            var counts = CountTwosAndThrees(line);
            if (counts.Item1)
            {
                twoCount++;
            }
            if (counts.Item2)
            {
                threeCount++;
            }
        }

        int checksum = twoCount * threeCount;
        Console.WriteLine(checksum);
    }

    static Tuple<bool, bool> CountTwosAndThrees(string id)
    {
        Dictionary<char, int> charCount = new Dictionary<char, int>();
        foreach (char c in id)
        {
            if (charCount.ContainsKey(c))
            {
                charCount[c]++;
            }
            else
            {
                charCount[c] = 1;
            }
        }

        bool hasTwos = false;
        bool hasThrees = false;

        foreach (int count in charCount.Values)
        {
            if (count == 2)
            {
                hasTwos = true;
            }
            else if (count == 3)
            {
                hasThrees = true;
            }
        }

        return Tuple.Create(hasTwos, hasThrees);
    }
}
