
using System;
using System.IO;
using System.Linq;

class RebootStep
{
    public string action;
    public int xStart;
    public int xEnd;
    public int yStart;
    public int yEnd;
    public int zStart;
    public int zEnd;
}

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        var rebootSteps = lines.Where(line => !string.IsNullOrEmpty(line)).Select(ParseRebootStep).ToList();

        int minCoord = -50;
        int maxCoord = 50;
        var cubeGrid = CreateCubeGrid(minCoord, maxCoord);
        ExecuteRebootSteps(cubeGrid, rebootSteps);
        int onCubes = CountOnCubes(cubeGrid);

        Console.WriteLine(onCubes);
    }

    static RebootStep ParseRebootStep(string line)
    {
        string[] parts = line.Split(' ');

        string action = parts[0];
        parts = parts[1].Split(',');
        string[] xRange = parts[0].Substring(2).Split("..");
        string[] yRange = parts[1].Substring(2).Split("..");
        string[] zRange = parts[2].Substring(2).Split("..");

        int xStart = int.Parse(xRange[0]);
        int xEnd = int.Parse(xRange[1]);
        int yStart = int.Parse(yRange[0]);
        int yEnd = int.Parse(yRange[1]);
        int zStart = int.Parse(zRange[0]);
        int zEnd = int.Parse(zRange[1]);

        return new RebootStep { action = action, xStart = xStart, xEnd = xEnd, yStart = yStart, yEnd = yEnd, zStart = zStart, zEnd = zEnd };
    }

    static bool[][][] CreateCubeGrid(int minCoord, int maxCoord)
    {
        int gridSize = maxCoord - minCoord + 1;
        var grid = new bool[gridSize][][];

        for (int i = 0; i < gridSize; i++)
        {
            grid[i] = new bool[gridSize][];
            for (int j = 0; j < gridSize; j++)
            {
                grid[i][j] = new bool[gridSize];
            }
        }

        return grid;
    }

    static void ExecuteRebootSteps(bool[][][] cubeGrid, RebootStep[] rebootSteps)
    {
        foreach (var step in rebootSteps)
        {
            if (!(step.xStart >= -50 && step.xEnd <= 50 && step.yStart >= -50 && step.yEnd <= 50 && step.zStart >= -50 && step.zEnd <= 50))
            {
                continue;
            }
            for (int x = step.xStart; x <= step.xEnd; x++)
            {
                for (int y = step.yStart; y <= step.yEnd; y++)
                {
                    for (int z = step.zStart; z <= step.zEnd; z++)
                    {
                        cubeGrid[x + 50][y + 50][z + 50] = step.action == "on";
                    }
                }
            }
        }
    }

    static int CountOnCubes(bool[][][] cubeGrid)
    {
        int count = 0;

        foreach (var plane in cubeGrid)
        {
            foreach (var row in plane)
            {
                count += row.Count(cell => cell);
            }
        }

        return count;
    }
}
