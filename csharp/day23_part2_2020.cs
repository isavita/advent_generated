
using System;
using System.IO;

class Program
{
    const int totalCups = 1000000;
    const int totalMoves = 10000000;

    static void Main()
    {
        string input = File.ReadAllText("input.txt");

        int[] cups = new int[totalCups + 1];
        int lastCup = 0;

        foreach (char c in input)
        {
            int cup = int.Parse(c.ToString());
            if (lastCup > 0)
            {
                cups[lastCup] = cup;
            }
            lastCup = cup;
        }

        for (int i = input.Length + 1; i <= totalCups; i++)
        {
            cups[lastCup] = i;
            lastCup = i;
        }
        cups[lastCup] = int.Parse(input[0].ToString());

        int currentCup = int.Parse(input[0].ToString());
        for (int i = 0; i < totalMoves; i++)
        {
            int pickup1 = cups[currentCup];
            int pickup2 = cups[pickup1];
            int pickup3 = cups[pickup2];

            cups[currentCup] = cups[pickup3];

            int destinationCup = currentCup - 1;
            if (destinationCup == 0)
            {
                destinationCup = totalCups;
            }
            while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3)
            {
                destinationCup--;
                if (destinationCup == 0)
                {
                    destinationCup = totalCups;
                }
            }

            cups[pickup3] = cups[destinationCup];
            cups[destinationCup] = pickup1;

            currentCup = cups[currentCup];
        }

        int cup1 = cups[1];
        int cup2 = cups[cup1];
        Console.WriteLine((long)cup1 * cup2);
    }
}
