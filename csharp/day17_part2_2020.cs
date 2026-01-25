
using System;
using System.IO;
using System.Linq;

public class Program
{
    private const int MaxSize = 30;
    private const int Cycles = 6;

    public static void Main(string[] args)
    {
        var grid = new bool[MaxSize, MaxSize, MaxSize, MaxSize];
        var input = File.ReadAllLines("input.txt");

        int mid = MaxSize / 2;
        int offset = mid - input.Length / 2;

        for (int i = 0; i < input.Length; i++)
        {
            for (int j = 0; j < input[i].Length; j++)
            {
                grid[mid, mid, offset + i, offset + j] = input[i][j] == '#';
            }
        }

        for (int cycle = 0; cycle < Cycles; cycle++)
        {
            var newGrid = new bool[MaxSize, MaxSize, MaxSize, MaxSize];

            for (int x = 1; x < MaxSize - 1; x++)
            {
                for (int y = 1; y < MaxSize - 1; y++)
                {
                    for (int z = 1; z < MaxSize - 1; z++)
                    {
                        for (int w = 1; w < MaxSize - 1; w++)
                        {
                            int activeNeighbors = CountActive(grid, x, y, z, w);
                            newGrid[x, y, z, w] = grid[x, y, z, w] ? (activeNeighbors == 2 || activeNeighbors == 3) : (activeNeighbors == 3);
                        }
                    }
                }
            }

            grid = newGrid;
        }

        int count = 0;
        for (int x = 0; x < MaxSize; x++)
        {
            for (int y = 0; y < MaxSize; y++)
            {
                for (int z = 0; z < MaxSize; z++)
                {
                    for (int w = 0; w < MaxSize; w++)
                    {
                        if (grid[x, y, z, w]) count++;
                    }
                }
            }
        }

        Console.WriteLine(count);
    }

    private static int CountActive(bool[,,,] grid, int x, int y, int z, int w)
    {
        int count = 0;
        for (int i = -1; i <= 1; i++)
        {
            for (int j = -1; j <= 1; j++)
            {
                for (int k = -1; k <= 1; k++)
                {
                    for (int l = -1; l <= 1; l++)
                    {
                        if (i == 0 && j == 0 && k == 0 && l == 0) continue;
                        if (x + i >= 0 && x + i < grid.GetLength(0) &&
                            y + j >= 0 && y + j < grid.GetLength(1) &&
                            z + k >= 0 && z + k < grid.GetLength(2) &&
                            w + l >= 0 && w + l < grid.GetLength(3) &&
                            grid[x + i, y + j, z + k, w + l])
                        {
                            count++;
                        }
                    }
                }
            }
        }
        return count;
    }
}
