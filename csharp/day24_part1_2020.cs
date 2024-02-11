
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    struct Coordinate
    {
        public int q, r;
    }

    static Dictionary<string, Coordinate> directions = new Dictionary<string, Coordinate>
    {
        { "e", new Coordinate { q = 1, r = 0 } },
        { "se", new Coordinate { q = 0, r = 1 } },
        { "sw", new Coordinate { q = -1, r = 1 } },
        { "w", new Coordinate { q = -1, r = 0 } },
        { "nw", new Coordinate { q = 0, r = -1 } },
        { "ne", new Coordinate { q = 1, r = -1 } }
    };

    static void Main()
    {
        StreamReader file = new StreamReader("input.txt");
        Dictionary<Coordinate, bool> blackTiles = new Dictionary<Coordinate, bool>();

        string line;
        while ((line = file.ReadLine()) != null)
        {
            Coordinate coord = new Coordinate { q = 0, r = 0 };

            for (int i = 0; i < line.Length; i++)
            {
                string dir;
                switch (line[i])
                {
                    case 'e':
                    case 'w':
                        dir = line[i].ToString();
                        break;
                    case 'n':
                    case 's':
                        dir = line.Substring(i, 2);
                        i++;
                        break;
                    default:
                        dir = "";
                        break;
                }

                Coordinate move = directions[dir];
                coord.q += move.q;
                coord.r += move.r;
            }

            blackTiles[coord] = !blackTiles.GetValueOrDefault(coord);
        }

        int count = 0;
        foreach (var black in blackTiles.Values)
        {
            if (black)
            {
                count++;
            }
        }
        Console.WriteLine(count);
    }
}
