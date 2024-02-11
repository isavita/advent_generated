
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] instructions = File.ReadAllLines("input.txt");
        string code = GetBathroomCode(instructions);
        Console.WriteLine(code);
    }

    static string GetBathroomCode(string[] instructions)
    {
        int[,] keypad = {
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        };
        int x = 1, y = 1; // Start at '5'
        string code = "";

        foreach (var instruction in instructions)
        {
            foreach (char move in instruction)
            {
                switch (move)
                {
                    case 'U':
                        if (x > 0) x--;
                        break;
                    case 'D':
                        if (x < 2) x++;
                        break;
                    case 'L':
                        if (y > 0) y--;
                        break;
                    case 'R':
                        if (y < 2) y++;
                        break;
                }
            }
            code += keypad[x, y].ToString();
        }

        return code;
    }
}
