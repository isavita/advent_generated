
using System;
using System.IO;

class Program
{
    static void Main()
    {
        StreamReader file = new StreamReader("input.txt");
        int totalScore = 0;

        string line;
        while ((line = file.ReadLine()) != null)
        {
            char opponent = line[0];
            char roundEnd = line[2];

            char yourMove = ' ';
            if (roundEnd == 'X')
            {
                yourMove = opponent == 'A' ? 'Z' : opponent == 'B' ? 'X' : 'Y';
            }
            else if (roundEnd == 'Y')
            {
                yourMove = opponent == 'A' ? 'X' : opponent == 'B' ? 'Y' : 'Z';
            }
            else
            {
                yourMove = opponent == 'A' ? 'Y' : opponent == 'B' ? 'Z' : 'X';
            }

            int score = yourMove == 'X' ? 1 : yourMove == 'Y' ? 2 : 3;

            if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X'))
            {
                score += 6;
            }
            else if ((opponent == 'A' && yourMove == 'X') || (opponent == 'B' && yourMove == 'Y') || (opponent == 'C' && yourMove == 'Z'))
            {
                score += 3;
            }

            totalScore += score;
        }

        file.Close();

        Console.WriteLine(totalScore);
    }
}
