
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static Dictionary<string, string> memo = new Dictionary<string, string>();

    static void Main()
    {
        var rules = new Dictionary<string, string>();

        using (StreamReader reader = new StreamReader("input.txt"))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                string[] parts = line.Split(" => ");
                rules[parts[0]] = parts[1];
            }
        }

        string[] grid = {
            ".#.",
            "..#",
            "###"
        };

        for (int i = 0; i < 18; i++)
        {
            int newSize, subSize;

            if (grid.Length % 2 == 0)
            {
                subSize = 2;
                newSize = grid.Length / 2 * 3;
            }
            else
            {
                subSize = 3;
                newSize = grid.Length / 3 * 4;
            }

            string[] newGrid = new string[newSize];
            for (int x = 0; x < newSize; x++)
            {
                newGrid[x] = "";
            }

            for (int y = 0; y < grid.Length; y += subSize)
            {
                for (int x = 0; x < grid.Length; x += subSize)
                {
                    List<string> square = new List<string>();
                    for (int dy = 0; dy < subSize; dy++)
                    {
                        square.Add(grid[y + dy].Substring(x, subSize));
                    }
                    string newSquare = Enhance(string.Join("/", square), rules);
                    string[] newSquareRows = newSquare.Split("/");
                    for (int dy = 0; dy < subSize + 1; dy++)
                    {
                        newGrid[y / subSize * (subSize + 1) + dy] += newSquareRows[dy];
                    }
                }
            }
            grid = newGrid;
        }

        int count = grid.Sum(row => row.Count(c => c == '#'));
        Console.WriteLine(count);
    }

    static string Enhance(string input, Dictionary<string, string> rules)
    {
        if (memo.ContainsKey(input))
        {
            return memo[input];
        }

        string original = input;
        for (int i = 0; i < 4; i++)
        {
            if (rules.TryGetValue(input, out string output))
            {
                memo[original] = output;
                return output;
            }
            input = Rotate(input);
        }
        input = Flip(input);
        for (int i = 0; i < 4; i++)
        {
            if (rules.TryGetValue(input, out string output))
            {
                memo[original] = output;
                return output;
            }
            input = Rotate(input);
        }
        return "";
    }

    static string Rotate(string input)
    {
        string[] parts = input.Split("/");
        int size = parts.Length;
        string[] newParts = new string[size];
        for (int x = 0; x < size; x++)
        {
            string newRow = "";
            for (int y = size - 1; y >= 0; y--)
            {
                newRow += parts[y][x];
            }
            newParts[x] = newRow;
        }
        return string.Join("/", newParts);
    }

    static string Flip(string input)
    {
        string[] parts = input.Split("/");
        for (int i = 0; i < parts.Length; i++)
        {
            parts[i] = new string(parts[i].Reverse().ToArray());
        }
        return string.Join("/", parts);
    }
}
