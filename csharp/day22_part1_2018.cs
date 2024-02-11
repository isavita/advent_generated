
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string data = File.ReadAllText("input.txt");
        (int depth, int[] target) = ParseInput(data);

        int[][] cave = MakeCaveSystem(depth, target);
        int riskLevel = CalculateRiskLevel(cave, target);
        Console.WriteLine(riskLevel);
    }

    static (int, int[]) ParseInput(string data)
    {
        string[] lines = data.Split("\n");
        int depth = int.Parse(lines[0].Split(" ")[1]);
        string[] coords = lines[1].Split(" ")[1].Split(",");
        int x = int.Parse(coords[0]);
        int y = int.Parse(coords[1]);
        return (depth, new int[] { x, y });
    }

    static int[][] MakeCaveSystem(int depth, int[] target)
    {
        int[][] cave = new int[target[1] + 1][];
        for (int y = 0; y < cave.Length; y++)
        {
            cave[y] = new int[target[0] + 1];
            for (int x = 0; x < cave[y].Length; x++)
            {
                int geologicIndex = 0;
                if ((x == 0 && y == 0) || (x == target[0] && y == target[1]))
                {
                    geologicIndex = 0;
                }
                else if (y == 0)
                {
                    geologicIndex = x * 16807;
                }
                else if (x == 0)
                {
                    geologicIndex = y * 48271;
                }
                else
                {
                    geologicIndex = cave[y][x - 1] * cave[y - 1][x];
                }
                cave[y][x] = (geologicIndex + depth) % 20183;
            }
        }
        return cave;
    }

    static int CalculateRiskLevel(int[][] cave, int[] target)
    {
        int riskLevel = 0;
        for (int y = 0; y <= target[1]; y++)
        {
            for (int x = 0; x <= target[0]; x++)
            {
                riskLevel += cave[y][x] % 3;
            }
        }
        return riskLevel;
    }
}
