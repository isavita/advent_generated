
using System;
using System.IO;

class Program
{
    const int gridSize = 1000;

    static void Main()
    {
        bool[][] grid = new bool[gridSize][];
        for (int i = 0; i < gridSize; i++)
        {
            grid[i] = new bool[gridSize];
        }

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                ProcessInstruction(line, grid);
            }
        }

        Console.WriteLine(CountLights(grid));
    }

    static void ProcessInstruction(string instruction, bool[][] grid)
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
                switch (parts[0])
                {
                    case "turn":
                        if (parts[1] == "on")
                            grid[x][y] = true;
                        else if (parts[1] == "off")
                            grid[x][y] = false;
                        break;
                    case "toggle":
                        grid[x][y] = !grid[x][y];
                        break;
                }
            }
        }
    }

    static int CountLights(bool[][] grid)
    {
        int count = 0;
        foreach (var row in grid)
        {
            foreach (var light in row)
            {
                if (light)
                {
                    count++;
                }
            }
        }
        return count;
    }
}
