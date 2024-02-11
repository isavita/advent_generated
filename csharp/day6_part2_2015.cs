
using System;
using System.IO;
using System.Linq;

class Program
{
    const int gridSize = 1000;

    static void Main()
    {
        int[][] grid = new int[gridSize][];
        for (int i = 0; i < gridSize; i++)
        {
            grid[i] = new int[gridSize];
        }

        string[] lines = File.ReadAllLines("input.txt");
        foreach (string line in lines)
        {
            ProcessInstruction(line, grid);
        }

        Console.WriteLine(TotalBrightness(grid));
    }

    static void ProcessInstruction(string instruction, int[][] grid)
    {
        string[] parts = instruction.Split(' ');
        int startX, startY, endX, endY;
        string[] startCoords = parts[parts.Length - 3].Split(',');
        string[] endCoords = parts[parts.Length - 1].Split(',');
        startX = int.Parse(startCoords[0]);
        startY = int.Parse(startCoords[1]);
        endX = int.Parse(endCoords[0]);
        endY = int.Parse(endCoords[1]);

        for (int x = startX; x <= endX; x++)
        {
            for (int y = startY; y <= endY; y++)
            {
                if (parts[0] == "turn" && parts[1] == "on")
                {
                    grid[x][y]++;
                }
                else if (parts[0] == "turn" && parts[1] == "off")
                {
                    if (grid[x][y] > 0)
                    {
                        grid[x][y]--;
                    }
                }
                else if (parts[0] == "toggle")
                {
                    grid[x][y] += 2;
                }
            }
        }
    }

    static int TotalBrightness(int[][] grid)
    {
        return grid.SelectMany(row => row).Sum();
    }
}
