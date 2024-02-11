
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        Dictionary<string, bool> holderMap = new Dictionary<string, bool>();
        Dictionary<string, bool> heldMap = new Dictionary<string, bool>();

        Regex re = new Regex("[a-z]+");

        foreach (string line in lines)
        {
            MatchCollection matches = re.Matches(line);
            string holder = matches[0].Value;
            holderMap[holder] = true;

            if (matches.Count > 1)
            {
                for (int i = 1; i < matches.Count; i++)
                {
                    heldMap[matches[i].Value] = true;
                }
            }
        }

        foreach (string holder in holderMap.Keys)
        {
            if (!heldMap.ContainsKey(holder))
            {
                Console.WriteLine(holder);
                return;
            }
        }
    }
}
