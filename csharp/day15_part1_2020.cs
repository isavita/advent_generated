
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt");
        var startingNumbers = input.Trim().Split(',');

        var lastSpoken = new System.Collections.Generic.Dictionary<int, int>();
        int lastNumber = 0, nextNumber = 0;

        for (int turn = 1; turn <= 2020; turn++)
        {
            if (turn - 1 < startingNumbers.Length)
            {
                lastNumber = int.Parse(startingNumbers[turn - 1]);
                lastSpoken[lastNumber] = turn;
                continue;
            }

            if (lastSpoken.TryGetValue(lastNumber, out int lastTurn) && lastTurn != turn - 1)
            {
                nextNumber = turn - 1 - lastTurn;
            }
            else
            {
                nextNumber = 0;
            }

            lastSpoken[lastNumber] = turn - 1;
            lastNumber = nextNumber;
        }

        Console.WriteLine(lastNumber);
    }
}
