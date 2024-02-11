
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        char[][] seatingArea = new char[lines.Length][];
        for (int i = 0; i < lines.Length; i++)
        {
            seatingArea[i] = lines[i].ToCharArray();
        }

        bool stabilized = false;
        while (!stabilized)
        {
            (seatingArea, stabilized) = SimulateSeating(seatingArea);
        }

        Console.WriteLine(CountOccupiedSeats(seatingArea));
    }

    static (char[][], bool) SimulateSeating(char[][] seatingArea)
    {
        int rows = seatingArea.Length;
        int cols = seatingArea[0].Length;
        char[][] newSeatingArea = new char[rows][];
        for (int i = 0; i < rows; i++)
        {
            newSeatingArea[i] = new char[cols];
            Array.Copy(seatingArea[i], newSeatingArea[i], cols);
        }
        bool stabilized = true;

        for (int i = 0; i < rows; i++)
        {
            for (int j = 0; j < cols; j++)
            {
                switch (seatingArea[i][j])
                {
                    case 'L':
                        if (CountAdjacentOccupied(seatingArea, i, j) == 0)
                        {
                            newSeatingArea[i][j] = '#';
                            stabilized = false;
                        }
                        break;
                    case '#':
                        if (CountAdjacentOccupied(seatingArea, i, j) >= 4)
                        {
                            newSeatingArea[i][j] = 'L';
                            stabilized = false;
                        }
                        break;
                }
            }
        }

        return (newSeatingArea, stabilized);
    }

    static int CountAdjacentOccupied(char[][] seatingArea, int row, int col)
    {
        int count = 0;
        for (int i = row - 1; i <= row + 1; i++)
        {
            for (int j = col - 1; j <= col + 1; j++)
            {
                if (i == row && j == col)
                {
                    continue;
                }
                if (i >= 0 && i < seatingArea.Length && j >= 0 && j < seatingArea[0].Length)
                {
                    if (seatingArea[i][j] == '#')
                    {
                        count++;
                    }
                }
            }
        }
        return count;
    }

    static int CountOccupiedSeats(char[][] seatingArea)
    {
        int count = 0;
        foreach (char[] row in seatingArea)
        {
            foreach (char seat in row)
            {
                if (seat == '#')
                {
                    count++;
                }
            }
        }
        return count;
    }
}
