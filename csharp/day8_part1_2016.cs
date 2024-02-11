
using System;
using System.IO;
using System.Text.RegularExpressions;

class Solution
{
    const int screenWidth = 50;
    const int screenHeight = 6;

    static void Main()
    {
        bool[][] screen = new bool[screenHeight][];
        for (int i = 0; i < screenHeight; i++)
        {
            screen[i] = new bool[screenWidth];
        }

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                ProcessInstruction(line, screen);
            }
        }

        Console.WriteLine(CountLitPixels(screen));
    }

    static void ProcessInstruction(string instruction, bool[][] screen)
    {
        Regex rectRegex = new Regex(@"rect (\d+)x(\d+)");
        Regex rotateRowRegex = new Regex(@"rotate row y=(\d+) by (\d+)");
        Regex rotateColumnRegex = new Regex(@"rotate column x=(\d+) by (\d+)");

        if (rectRegex.IsMatch(instruction))
        {
            Match match = rectRegex.Match(instruction);
            int a = int.Parse(match.Groups[1].Value);
            int b = int.Parse(match.Groups[2].Value);
            Rect(screen, a, b);
        }
        else if (rotateRowRegex.IsMatch(instruction))
        {
            Match match = rotateRowRegex.Match(instruction);
            int a = int.Parse(match.Groups[1].Value);
            int b = int.Parse(match.Groups[2].Value);
            RotateRow(screen, a, b);
        }
        else if (rotateColumnRegex.IsMatch(instruction))
        {
            Match match = rotateColumnRegex.Match(instruction);
            int a = int.Parse(match.Groups[1].Value);
            int b = int.Parse(match.Groups[2].Value);
            RotateColumn(screen, a, b);
        }
    }

    static void Rect(bool[][] screen, int a, int b)
    {
        for (int y = 0; y < b; y++)
        {
            for (int x = 0; x < a; x++)
            {
                screen[y][x] = true;
            }
        }
    }

    static void RotateRow(bool[][] screen, int row, int shift)
    {
        bool[] temp = new bool[screenWidth];
        for (int i = 0; i < screenWidth; i++)
        {
            temp[(i + shift) % screenWidth] = screen[row][i];
        }
        screen[row] = temp;
    }

    static void RotateColumn(bool[][] screen, int col, int shift)
    {
        bool[] temp = new bool[screenHeight];
        for (int i = 0; i < screenHeight; i++)
        {
            temp[(i + shift) % screenHeight] = screen[i][col];
        }
        for (int i = 0; i < screenHeight; i++)
        {
            screen[i][col] = temp[i];
        }
    }

    static int CountLitPixels(bool[][] screen)
    {
        int count = 0;
        foreach (bool[] row in screen)
        {
            foreach (bool pixel in row)
            {
                if (pixel)
                {
                    count++;
                }
            }
        }
        return count;
    }
}
