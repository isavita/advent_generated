
using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int sum = SumOfPartNumbers(lines);
        Console.WriteLine(sum);
    }

    static int SumOfPartNumbers(string[] lines)
    {
        int sum = 0;
        bool[,] visited = new bool[lines.Length, lines[0].Length];

        for (int y = 0; y < lines.Length; y++)
        {
            for (int x = 0; x < lines[y].Length; x++)
            {
                if (!visited[y, x] && char.IsDigit(lines[y][x]))
                {
                    Tuple<int, int> numberAndLength = ExtractNumber(lines, x, y);
                    int number = numberAndLength.Item1;
                    int length = numberAndLength.Item2;

                    if (IsAdjacentToSymbol(lines, x, y, length))
                    {
                        sum += number;
                    }

                    for (int i = 0; i < length; i++)
                    {
                        visited[y, x + i] = true;
                    }
                }
            }
        }

        return sum;
    }

    static Tuple<int, int> ExtractNumber(string[] lines, int x, int y)
    {
        string numberStr = "";
        while (x < lines[y].Length && char.IsDigit(lines[y][x]))
        {
            numberStr += lines[y][x];
            x++;
        }

        int number = int.Parse(numberStr);
        return Tuple.Create(number, numberStr.Length);
    }

    static bool IsAdjacentToSymbol(string[] lines, int x, int y, int length)
    {
        for (int i = 0; i < length; i++)
        {
            if (CheckAdjacent(lines, x + i, y))
            {
                return true;
            }
        }

        return false;
    }

    static bool CheckAdjacent(string[] lines, int x, int y)
    {
        for (int dy = -1; dy <= 1; dy++)
        {
            for (int dx = -1; dx <= 1; dx++)
            {
                int adjX = x + dx;
                int adjY = y + dy;
                if (adjY >= 0 && adjY < lines.Length && adjX >= 0 && adjX < lines[adjY].Length)
                {
                    if (!char.IsDigit(lines[adjY][adjX]) && lines[adjY][adjX] != '.')
                    {
                        return true;
                    }
                }
            }
        }

        return false;
    }
}
