
using System;
using System.IO;

class Solution
{
    const int gridSize = 100;
    const int steps = 100;

    static int CountOnNeighbors(bool[][] grid, int x, int y)
    {
        int on = 0;
        for (int dx = -1; dx <= 1; dx++)
        {
            for (int dy = -1; dy <= 1; dy++)
            {
                if (dx == 0 && dy == 0)
                {
                    continue;
                }
                int nx = x + dx;
                int ny = y + dy;
                if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx][ny])
                {
                    on++;
                }
            }
        }
        return on;
    }

    static bool[][] Step(bool[][] grid)
    {
        bool[][] newGrid = new bool[gridSize][];
        for (int i = 0; i < gridSize; i++)
        {
            newGrid[i] = new bool[gridSize];
        }

        for (int x = 0; x < gridSize; x++)
        {
            for (int y = 0; y < gridSize; y++)
            {
                int onNeighbors = CountOnNeighbors(grid, x, y);
                if (grid[x][y])
                {
                    newGrid[x][y] = onNeighbors == 2 || onNeighbors == 3;
                }
                else
                {
                    newGrid[x][y] = onNeighbors == 3;
                }
            }
        }

        return newGrid;
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        bool[][] grid = new bool[gridSize][];
        for (int i = 0; i < gridSize; i++)
        {
            grid[i] = new bool[gridSize];
        }

        for (int y = 0; y < gridSize; y++)
        {
            string line = lines[y];
            for (int x = 0; x < gridSize; x++)
            {
                grid[x][y] = line[x] == '#';
            }
        }

        for (int i = 0; i < steps; i++)
        {
            grid = Step(grid);
        }

        int onCount = 0;
        foreach (bool[] row in grid)
        {
            foreach (bool light in row)
            {
                if (light)
                {
                    onCount++;
                }
            }
        }

        Console.WriteLine(onCount);
    }
}
