
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        int x = 0, y = 0;
        for (int i = 0; i < lines[0].Length; i++)
        {
            if (lines[0][i] == '|')
            {
                x = i;
                break;
            }
        }

        int dx = 0, dy = 1;
        string letters = "";

        while (x >= 0 && x < lines[0].Length && y >= 0 && y < lines.Length)
        {
            char cell = lines[y][x];

            if (cell == ' ')
            {
                break;
            }

            if (cell >= 'A' && cell <= 'Z')
            {
                letters += cell;
            }

            if (cell == '+')
            {
                if (dx == 0)
                {
                    if (x > 0 && (lines[y][x - 1] == '-' || (lines[y][x - 1] >= 'A' && lines[y][x - 1] <= 'Z')))
                    {
                        dx = -1;
                        dy = 0;
                    }
                    else
                    {
                        dx = 1;
                        dy = 0;
                    }
                }
                else
                {
                    if (y > 0 && (lines[y - 1][x] == '|' || (lines[y - 1][x] >= 'A' && lines[y - 1][x] <= 'Z')))
                    {
                        dx = 0;
                        dy = -1;
                    }
                    else
                    {
                        dx = 0;
                        dy = 1;
                    }
                }
            }

            x += dx;
            y += dy;
        }

        Console.WriteLine(letters);
    }
}
