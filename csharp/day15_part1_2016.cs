
using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

class Disc
{
    public int totalPositions;
    public int startPosition;
}

class Program
{
    static void Main()
    {
        List<Disc> discs = new List<Disc>();
        string line;
        StreamReader file = new StreamReader("input.txt");
        Regex discRegex = new Regex(@"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).");

        while ((line = file.ReadLine()) != null)
        {
            Match match = discRegex.Match(line);
            int totalPositions = int.Parse(match.Groups[2].Value);
            int startPosition = int.Parse(match.Groups[3].Value);
            discs.Add(new Disc { totalPositions = totalPositions, startPosition = startPosition });
        }

        int time = 0;
        while (true)
        {
            if (CheckDiscs(discs, time))
            {
                Console.WriteLine(time);
                break;
            }
            time++;
        }
    }

    static bool CheckDiscs(List<Disc> discs, int time)
    {
        for (int i = 0; i < discs.Count; i++)
        {
            Disc disc = discs[i];
            int position = (disc.startPosition + time + i + 1) % disc.totalPositions;
            if (position != 0)
            {
                return false;
            }
        }
        return true;
    }
}
