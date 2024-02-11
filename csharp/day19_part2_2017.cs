
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        char[][] grid = new char[lines.Length][];
        for (int i = 0; i < lines.Length; i++)
        {
            grid[i] = lines[i].ToCharArray();
        }

        int x = 0, y = 0;
        for (int i = 0; i < grid[0].Length; i++)
        {
            if (grid[0][i] == '|')
            {
                x = i;
                break;
            }
        }

        int dx = 0, dy = 1;
        int steps = 0;

        while (x >= 0 && x < grid[0].Length && y >= 0 && y < grid.Length)
        {
            char cell = grid[y][x];

            if (cell == ' ')
            {
                break;
            }

            steps++;

            if (cell == '+')
            {
                if (dx == 0)
                {
                    if (x > 0 && (grid[y][x - 1] == '-' || (grid[y][x - 1] >= 'A' && grid[y][x - 1] <= 'Z')))
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
                    if (y > 0 && (grid[y - 1][x] == '|' || (grid[y - 1][x] >= 'A' && grid[y - 1][x] <= 'Z')))
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

        Console.WriteLine(steps);
    }
}
