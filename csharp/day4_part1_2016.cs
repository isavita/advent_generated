
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int sumOfSectorIDs = 0;
        
        foreach (string line in lines)
        {
            if (IsRealRoom(line))
            {
                sumOfSectorIDs += GetSectorID(line);
            }
        }

        Console.WriteLine(sumOfSectorIDs);
    }

    static bool IsRealRoom(string room)
    {
        string[] parts = room.Split('[');
        string checksum = parts[1].TrimEnd(']');
        string[] encryptedName = parts[0].Split('-').Take(parts[0].Split('-').Length - 1).ToArray();

        Dictionary<char, int> letterCounts = new Dictionary<char, int>();
        foreach (string part in encryptedName)
        {
            foreach (char letter in part)
            {
                if (letterCounts.ContainsKey(letter))
                {
                    letterCounts[letter]++;
                }
                else
                {
                    letterCounts[letter] = 1;
                }
            }
        }

        var counts = letterCounts.OrderByDescending(x => x.Value).ThenBy(x => x.Key).ToList();

        for (int i = 0; i < checksum.Length; i++)
        {
            if (checksum[i] != counts[i].Key)
            {
                return false;
            }
        }

        return true;
    }

    static int GetSectorID(string room)
    {
        string[] parts = room.Split('-');
        string sectorIDPart = parts[parts.Length - 1];
        int sectorID = int.Parse(sectorIDPart.Split('[')[0]);
        return sectorID;
    }
}
