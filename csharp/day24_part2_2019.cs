
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    const int Side = 5;
    const int Square = Side * Side;

    static bool[] Parse()
    {
        bool[] res = new bool[Square];
        string[] lines = File.ReadAllLines("input.txt");

        for (int row = 0; row < Side; row++)
        {
            for (int col = 0; col < Side; col++)
            {
                if (lines[row][col] == '#')
                {
                    res[row * Side + col] = true;
                }
                else
                {
                    res[row * Side + col] = false;
                }
            }
        }

        return res;
    }

    static Dictionary<int, bool[]> Next2(Dictionary<int, bool[]> space)
    {
        Dictionary<int, bool[]> newSpace = new Dictionary<int, bool[]>();

        Tuple<int, int> minMaxLevel = MinMaxLevel(space);
        int minLevel = minMaxLevel.Item1 - 1;
        int maxLevel = minMaxLevel.Item2 + 1;

        for (int level = minLevel; level <= maxLevel; level++)
        {
            newSpace[level] = new bool[Square];

            for (int cell = 0; cell < Square; cell++)
            {
                if (cell == 12)
                {
                    continue;
                }

                int row = cell / Side;
                int col = cell % Side;
                int neighbours = 0;

                if (row == 0 && Infested(space, level - 1, 7))
                {
                    neighbours++;
                }

                if (col == 0 && Infested(space, level - 1, 11))
                {
                    neighbours++;
                }

                if (col == 4 && Infested(space, level - 1, 13))
                {
                    neighbours++;
                }

                if (row == 4 && Infested(space, level - 1, 17))
                {
                    neighbours++;
                }

                if (cell == 7)
                {
                    for (int i = 0; i < Side; i++)
                    {
                        if (Infested(space, level + 1, i))
                        {
                            neighbours++;
                        }
                    }
                }

                if (cell == 11)
                {
                    for (int i = 0; i < Side; i++)
                    {
                        if (Infested(space, level + 1, 5 * i))
                        {
                            neighbours++;
                        }
                    }
                }

                if (cell == 13)
                {
                    for (int i = 0; i < Side; i++)
                    {
                        if (Infested(space, level + 1, 5 * i + Side - 1))
                        {
                            neighbours++;
                        }
                    }
                }

                if (cell == 17)
                {
                    for (int i = 0; i < Side; i++)
                    {
                        if (Infested(space, level + 1, (Side - 1) * Side + i))
                        {
                            neighbours++;
                        }
                    }
                }

                if (row > 0 && cell != 17 && Infested(space, level, cell - Side))
                {
                    neighbours++;
                }

                if (col > 0 && cell != 13 && Infested(space, level, cell - 1))
                {
                    neighbours++;
                }

                if (col < Side - 1 && cell != 11 && Infested(space, level, cell + 1))
                {
                    neighbours++;
                }

                if (row < Side - 1 && cell != 7 && Infested(space, level, cell + Side))
                {
                    neighbours++;
                }

                if (Infested(space, level, cell) && neighbours != 1)
                {
                    newSpace[level][cell] = false;
                    continue;
                }

                if (!Infested(space, level, cell) && (neighbours == 1 || neighbours == 2))
                {
                    newSpace[level][cell] = true;
                    continue;
                }

                newSpace[level][cell] = Infested(space, level, cell);
            }
        }

        Clean(newSpace);

        return newSpace;
    }

    static void Clean(Dictionary<int, bool[]> space)
    {
        Tuple<int, int> minMax = MinMaxLevel(space);
        int min = minMax.Item1;
        int max = minMax.Item2;
        int countMin = 0;
        int countMax = 0;

        for (int cell = 0; cell < Square; cell++)
        {
            if (space[min][cell])
            {
                countMin++;
            }
            if (space[max][cell])
            {
                countMax++;
            }
        }

        if (countMin == 0)
        {
            space.Remove(min);
        }
        if (countMax == 0)
        {
            space.Remove(max);
        }
    }

    static bool Infested(Dictionary<int, bool[]> space, int level, int cell)
    {
        if (!space.ContainsKey(level))
        {
            return false;
        }
        return space[level][cell];
    }

    static Tuple<int, int> MinMaxLevel(Dictionary<int, bool[]> space)
    {
        int min = int.MaxValue;
        int max = int.MinValue;

        foreach (int level in space.Keys)
        {
            if (level < min)
            {
                min = level;
            }
            if (level > max)
            {
                max = level;
            }
        }

        return Tuple.Create(min, max);
    }

    static void Main()
    {
        bool[] input = Parse();
        Dictionary<int, bool[]> space = new Dictionary<int, bool[]>
        {
            { 0, input }
        };

        for (int i = 0; i < 200; i++)
        {
            space = Next2(space);
        }

        int count = 0;
        foreach (bool[] grid in space.Values)
        {
            foreach (bool cell in grid)
            {
                if (cell)
                {
                    count++;
                }
            }
        }

        Console.WriteLine(count);
    }
}
