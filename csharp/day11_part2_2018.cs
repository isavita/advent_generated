
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        int serial = int.Parse(input);

        const int gridSize = 300;
        int[][] grid = new int[gridSize][];
        for (int i = 0; i < gridSize; i++)
        {
            grid[i] = new int[gridSize];
        }

        for (int y = 0; y < gridSize; y++)
        {
            for (int x = 0; x < gridSize; x++)
            {
                int rackID = x + 11;
                int powerLevel = rackID * (y + 1) + serial;
                powerLevel *= rackID;
                powerLevel = (powerLevel / 100) % 10;
                powerLevel -= 5;
                grid[y][x] = powerLevel;
            }
        }

        int[][] summedAreaTable = new int[gridSize][];
        for (int i = 0; i < gridSize; i++)
        {
            summedAreaTable[i] = new int[gridSize];
        }

        for (int y = 0; y < gridSize; y++)
        {
            for (int x = 0; x < gridSize; x++)
            {
                int value = grid[y][x];
                int left = x > 0 ? summedAreaTable[y][x - 1] : 0;
                int top = y > 0 ? summedAreaTable[y - 1][x] : 0;
                int topLeft = (x > 0 && y > 0) ? summedAreaTable[y - 1][x - 1] : 0;

                summedAreaTable[y][x] = value + left + top - topLeft;
            }
        }

        int maxPower = int.MinValue;
        int maxX = 0, maxY = 0, maxSize = 0;
        for (int size = 1; size <= gridSize; size++)
        {
            for (int y = 0; y < gridSize - size + 1; y++)
            {
                for (int x = 0; x < gridSize - size + 1; x++)
                {
                    int totalPower = summedAreaTable[y + size - 1][x + size - 1];
                    if (y > 0)
                    {
                        totalPower -= summedAreaTable[y - 1][x + size - 1];
                    }
                    if (x > 0)
                    {
                        totalPower -= summedAreaTable[y + size - 1][x - 1];
                    }
                    if (y > 0 && x > 0)
                    {
                        totalPower += summedAreaTable[y - 1][x - 1];
                    }

                    if (totalPower > maxPower)
                    {
                        maxPower = totalPower;
                        maxX = x + 1;
                        maxY = y + 1;
                        maxSize = size;
                    }
                }
            }
        }

        Console.WriteLine($"{maxX},{maxY},{maxSize}");
    }
}
