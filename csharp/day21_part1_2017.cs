
using System;
using System.Collections.Generic;
using System.IO;

class Solution
{
    static void Main()
    {
        Dictionary<string, string> rules = new Dictionary<string, string>();

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
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

        for (int i = 0; i < 5; i++)
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
                    for (int dy = 0; dy < newSquareRows.Length; dy++)
                    {
                        newGrid[y / subSize * (subSize + 1) + dy] += newSquareRows[dy];
                    }
                }
            }
            grid = newGrid;
        }

        int count = 0;
        foreach (string row in grid)
        {
            foreach (char pixel in row)
            {
                if (pixel == '#')
                {
                    count++;
                }
            }
        }
        Console.WriteLine(count);
    }

    static string Enhance(string input, Dictionary<string, string> rules)
    {
        for (int i = 0; i < 4; i++)
        {
            if (rules.ContainsKey(input))
            {
                return rules[input];
            }
            input = Rotate(input);
        }
        input = Flip(input);
        for (int i = 0; i < 4; i++)
        {
            if (rules.ContainsKey(input))
            {
                return rules[input];
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
            parts[i] = Reverse(parts[i]);
        }
        return string.Join("/", parts);
    }

    static string Reverse(string input)
    {
        char[] chars = input.ToCharArray();
        Array.Reverse(chars);
        return new string(chars);
    }
}
