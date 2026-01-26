
using System;
using System.IO;
using System.Collections.Generic;

public class Chiton
{
    public static void Main()
    {
        try
        {
            int[][] grid = ReadInput("input.txt");
            Console.WriteLine("Lowest total risk (Part 1): " + FindLowestRiskPath(grid));

            int[][] expandedGrid = ExpandGrid(grid, 5);
            Console.WriteLine("Lowest total risk (Part 2): " + FindLowestRiskPath(expandedGrid));
        }
        catch (FileNotFoundException e)
        {
            Console.Error.WriteLine("Error reading input file: " + e.Message);
        }
    }

    private static int[][] ReadInput(string filename)
    {
        string[] lines = File.ReadAllLines(filename);
        int[][] grid = new int[lines.Length][];

        for (int i = 0; i < lines.Length; i++)
        {
            grid[i] = new int[lines[i].Length];
            for (int j = 0; j < lines[i].Length; j++)
            {
                grid[i][j] = lines[i][j] - '0';
            }
        }

        return grid;
    }

    private static int FindLowestRiskPath(int[][] grid)
    {
        int rows = grid.Length;
        int cols = grid[0].Length;
        int[,] dist = new int[rows, cols];
        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                dist[i, j] = int.MaxValue;
            }
        }
        dist[0, 0] = 0;

        var pq = new SortedSet<(int, int, int)>();
        pq.Add((0, 0, 0));

        int[][] dirs = new int[][] { new int[] { -1, 0 }, new int[] { 1, 0 }, new int[] { 0, -1 }, new int[] { 0, 1 } };

        while (pq.Count > 0)
        {
            var (row, col, cost) = pq.Min;
            pq.Remove((row, col, cost));

            if (row == rows - 1 && col == cols - 1)
            {
                return cost;
            }

            if (cost > dist[row, col])
            {
                continue;
            }

            foreach (var dir in dirs)
            {
                int newRow = row + dir[0];
                int newCol = col + dir[1];

                if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols)
                {
                    int newCost = cost + grid[newRow][newCol];
                    if (newCost < dist[newRow, newCol])
                    {
                        dist[newRow, newCol] = newCost;
                        pq.Add((newRow, newCol, newCost));
                    }
                }
            }
        }

        return -1; // Should not reach here if a path exists
    }

    private static int[][] ExpandGrid(int[][] grid, int factor)
    {
        int originalRows = grid.Length;
        int originalCols = grid[0].Length;
        int newRows = originalRows * factor;
        int newCols = originalCols * factor;
        int[][] expandedGrid = new int[newRows][];

        for (int i = 0; i < newRows; i++)
        {
            expandedGrid[i] = new int[newCols];
            for (int j = 0; j < newCols; j++)
            {
                int originalValue = grid[i % originalRows][j % originalCols];
                int increment = i / originalRows + j / originalCols;
                int newValue = (originalValue + increment - 1) % 9 + 1;
                expandedGrid[i][j] = newValue;
            }
        }

        return expandedGrid;
    }
}
