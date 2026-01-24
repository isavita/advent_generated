
using System;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        var line = File.ReadAllText("input.txt").Trim();
        var m = Regex.Match(line, @"(\d+)\s+players;.*?(\d+)\s+points");
        int players = int.Parse(m.Groups[1].Value);
        int lastMarble = int.Parse(m.Groups[2].Value) * 100;

        var scores = new long[players];

        // circular doubly‑linked list implemented with index arrays
        int max = lastMarble;
        int[] next = new int[max + 1];
        int[] prev = new int[max + 1];
        next[0] = 0;
        prev[0] = 0;
        int current = 0;

        for (int marble = 1; marble <= max; marble++)
        {
            if (marble % 23 == 0)
            {
                int player = marble % players;
                for (int i = 0; i < 7; i++) current = prev[current];
                scores[player] += marble + current;          // current == marble number to remove
                int left = prev[current];
                int right = next[current];
                next[left] = right;
                prev[right] = left;
                current = right;
            }
            else
            {
                int left = next[current];     // one step clockwise
                int right = next[left];       // position for insertion
                next[left] = marble;
                prev[marble] = left;
                next[marble] = right;
                prev[right] = marble;
                current = marble;
            }
        }

        long maxScore = 0;
        foreach (var s in scores)
            if (s > maxScore) maxScore = s;

        Console.WriteLine(maxScore);
    }
}
