
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string directions = File.ReadAllText("input.txt");
        Dictionary<Tuple<int, int>, bool> visitedHouses = new Dictionary<Tuple<int, int>, bool>();
        int xSanta = 0, ySanta = 0; // Santa's starting position
        int xRobo = 0, yRobo = 0;   // Robo-Santa's starting position
        bool isSantaTurn = true;    // Flag to alternate turns between Santa and Robo-Santa

        visitedHouses.Add(Tuple.Create(xSanta, ySanta), true);

        foreach (char dir in directions)
        {
            int x = isSantaTurn ? xSanta : xRobo;
            int y = isSantaTurn ? ySanta : yRobo;

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

            Tuple<int, int> position = Tuple.Create(x, y);
            if (!visitedHouses.ContainsKey(position))
            {
                visitedHouses.Add(position, true);
            }

            if (isSantaTurn)
            {
                xSanta = x;
                ySanta = y;
            }
            else
            {
                xRobo = x;
                yRobo = y;
            }

            isSantaTurn = !isSantaTurn; // Switch turns
        }

        Console.WriteLine(visitedHouses.Count);
    }
}
