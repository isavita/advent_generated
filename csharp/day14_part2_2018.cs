
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();

        List<int> scoreboard = new List<int> { 3, 7 };
        int elf1 = 0;
        int elf2 = 1;
        int inputLen = input.Length;
        int[] inputSequence = input.Select(c => c - '0').ToArray();

        while (true)
        {
            int newScore = scoreboard[elf1] + scoreboard[elf2];
            if (newScore >= 10)
            {
                scoreboard.Add(newScore / 10);
                if (CheckSequence(scoreboard, inputSequence))
                {
                    break;
                }
            }
            scoreboard.Add(newScore % 10);
            if (CheckSequence(scoreboard, inputSequence))
            {
                break;
            }

            elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.Count;
            elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.Count;
        }

        Console.WriteLine(scoreboard.Count - inputLen);
    }

    static bool CheckSequence(List<int> scoreboard, int[] sequence)
    {
        if (scoreboard.Count < sequence.Length)
        {
            return false;
        }

        int start = scoreboard.Count - sequence.Length;
        return !sequence.Where((t, i) => scoreboard[start + i] != t).Any();
    }
}
