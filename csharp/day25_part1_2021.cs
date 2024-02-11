
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

        Console.WriteLine(FindSafeStep(grid));
    }

    static int FindSafeStep(char[][] grid)
    {
        int step = 0;
        while (true)
        {
            bool eastMoved = MoveEast(grid);
            bool southMoved = MoveSouth(grid);
            step++;

            if (!eastMoved && !southMoved)
            {
                break;
            }
        }
        return step;
    }

    static bool MoveEast(char[][] grid)
    {
        bool moved = false;
        int height = grid.Length;
        int width = grid[0].Length;
        char[][] oldPositions = new char[height][];

        for (int i = 0; i < height; i++)
        {
            oldPositions[i] = new char[width];
        }

        for (int y = 0; y < height; y++)
        {
            for (int x = 0; x < width; x++)
            {
                if (grid[y][x] == '>')
                {
                    int nextX = (x + 1) % width;
                    if (grid[y][nextX] == '.')
                    {
                        oldPositions[y][x] = '.';
                        grid[y][nextX] = '>';
                        x++;
                        moved = true;
                    }
                }
            }
        }

        FreeEmptyPositions(grid, oldPositions);
        return moved;
    }

    static bool MoveSouth(char[][] grid)
    {
        bool moved = false;
        int height = grid.Length;
        int width = grid[0].Length;
        char[][] oldPositions = new char[height][];

        for (int i = 0; i < height; i++)
        {
            oldPositions[i] = new char[width];
        }

        for (int x = 0; x < width; x++)
        {
            for (int y = 0; y < height; y++)
            {
                if (grid[y][x] == 'v')
                {
                    int nextY = (y + 1) % height;
                    if (grid[nextY][x] == '.')
                    {
                        oldPositions[y][x] = '.';
                        grid[nextY][x] = 'v';
                        y++;
                        moved = true;
                    }
                }
            }
        }

        FreeEmptyPositions(grid, oldPositions);
        return moved;
    }

    static void FreeEmptyPositions(char[][] grid, char[][] oldPositions)
    {
        for (int y = 0; y < grid.Length; y++)
        {
            for (int x = 0; x < grid[0].Length; x++)
            {
                if (oldPositions[y][x] == '.')
                {
                    grid[y][x] = '.';
                }
            }
        }
    }
}
