
using System;
using System.IO;
using System.Text.RegularExpressions;

class Disc
{
    public int totalPositions;
    public int startPosition;
}

class Program
{
    static void Main()
    {
        Disc[] discs = ReadInput("input.txt");

        // Add the new disc as per Part Two's requirement
        Array.Resize(ref discs, discs.Length + 1);
        discs[discs.Length - 1] = new Disc { totalPositions = 11, startPosition = 0 };

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

    static Disc[] ReadInput(string filename)
    {
        string[] lines = File.ReadAllLines(filename);
        Disc[] discs = new Disc[lines.Length];
        Regex discRegex = new Regex(@"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).");

        for (int i = 0; i < lines.Length; i++)
        {
            Match match = discRegex.Match(lines[i]);
            int totalPositions = int.Parse(match.Groups[2].Value);
            int startPosition = int.Parse(match.Groups[3].Value);
            discs[i] = new Disc { totalPositions = totalPositions, startPosition = startPosition };
        }

        return discs;
    }

    static bool CheckDiscs(Disc[] discs, int time)
    {
        for (int i = 0; i < discs.Length; i++)
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
