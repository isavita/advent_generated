
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string directions = File.ReadAllText("input.txt");
        var visitedHouses = new Dictionary<(int, int), bool>();
        int x = 0, y = 0; // Santa's starting position

        visitedHouses[(x, y)] = true;

        foreach (char dir in directions)
        {
            switch (dir)
            {
                case '^':
                    y++; // Move north
                    break;
                case 'v':
                    y--; // Move south
                    break;
                case '>':
                    x++; // Move east
                    break;
                case '<':
                    x--; // Move west
                    break;
            }

            visitedHouses[(x, y)] = true;
        }

        Console.WriteLine(visitedHouses.Count);
    }
}
