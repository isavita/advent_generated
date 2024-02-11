
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int player1Start = int.Parse(lines[0].Substring(28).Trim());
        int player2Start = int.Parse(lines[1].Substring(28).Trim());
        int player1Pos = player1Start;
        int player2Pos = player2Start;
        int player1Score = 0;
        int player2Score = 0;
        int dieRoll = 1;
        int rollCount = 0;

        while (true)
        {
            // Player 1
            int rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100;
            rollCount += 3;
            dieRoll += 3;

            player1Pos = (player1Pos + rolls - 1) % 10 + 1;
            player1Score += player1Pos;

            if (player1Score >= 1000)
            {
                Console.WriteLine("Result: " + player2Score * rollCount);
                break;
            }

            // Player 2
            rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100;
            rollCount += 3;
            dieRoll += 3;

            player2Pos = (player2Pos + rolls - 1) % 10 + 1;
            player2Score += player2Pos;

            if (player2Score >= 1000)
            {
                Console.WriteLine(player1Score * rollCount);
                break;
            }
        }
    }
}
