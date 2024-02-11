
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string inputString = File.ReadAllText("input.txt");
        int input = int.Parse(inputString);

        var scoreboard = new System.Collections.Generic.List<int> { 3, 7 };
        int elf1 = 0;
        int elf2 = 1;

        while (scoreboard.Count < input + 10)
        {
            int newScore = scoreboard[elf1] + scoreboard[elf2];
            if (newScore >= 10)
            {
                scoreboard.Add(newScore / 10);
            }
            scoreboard.Add(newScore % 10);

            elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.Count;
            elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.Count;
        }

        for (int i = input; i < input + 10; i++)
        {
            Console.Write(scoreboard[i]);
        }
        Console.WriteLine();
    }
}
