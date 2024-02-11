
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    const int Side = 5;
    const int Square = Side * Side;

    static bool[] Parse()
    {
        bool[] res = new bool[Square];

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            int row = 0;
            while ((line = sr.ReadLine()) != null)
            {
                for (int col = 0; col < Side; col++)
                {
                    if (line[col] == '#')
                    {
                        res[row * Side + col] = true;
                    }
                    else
                    {
                        res[row * Side + col] = false;
                    }
                }
                row++;
            }
        }

        return res;
    }

    static void Main()
    {
        Dictionary<bool[], bool> appeared = new Dictionary<bool[], bool>(new BoolArrayComparer());

        bool[] grid = Parse();
        appeared.Add(grid, true);
        while (true)
        {
            grid = Next1(grid);
            if (appeared.ContainsKey(grid))
            {
                Console.WriteLine(Biodiversity(grid));
                return;
            }
            appeared.Add(grid, true);
        }
    }

    static bool[] Next1(bool[] grid)
    {
        bool[] newGrid = new bool[Square];

        for (int i = 0; i < Square; i++)
        {
            int row = i / Side;
            int col = i % Side;
            int neighbours = 0;

            if (row > 0 && grid[i - Side])
            {
                neighbours++;
            }
            if (row < Side - 1 && grid[i + Side])
            {
                neighbours++;
            }
            if (col > 0 && grid[i - 1])
            {
                neighbours++;
            }
            if (col < Side - 1 && grid[i + 1])
            {
                neighbours++;
            }

            if (grid[i] && neighbours != 1)
            {
                newGrid[i] = false;
                continue;
            }

            if (!grid[i] && (neighbours == 1 || neighbours == 2))
            {
                newGrid[i] = true;
                continue;
            }

            newGrid[i] = grid[i];
        }

        return newGrid;
    }

    static int Biodiversity(bool[] grid)
    {
        int bio = 0;
        for (int i = 0; i < Square; i++)
        {
            if (grid[i])
            {
                bio += 1 << i;
            }
        }
        return bio;
    }

    class BoolArrayComparer : IEqualityComparer<bool[]>
    {
        public bool Equals(bool[] x, bool[] y)
        {
            if (x.Length != y.Length)
            {
                return false;
            }

            for (int i = 0; i < x.Length; i++)
            {
                if (x[i] != y[i])
                {
                    return false;
                }
            }

            return true;
        }

        public int GetHashCode(bool[] obj)
        {
            int hash = 17;
            foreach (bool b in obj)
            {
                hash = hash * 31 + b.GetHashCode();
            }
            return hash;
        }
    }
}
