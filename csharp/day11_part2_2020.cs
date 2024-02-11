
using System;
using System.IO;
using System.Linq;

class Point
{
    public int x, y;
    public Point(int x, int y)
    {
        this.x = x;
        this.y = y;
    }
}

class Program
{
    static Point[] directions = new Point[]
    {
        new Point(-1, -1), new Point(0, -1), new Point(1, -1),
        new Point(-1, 0), /*new Point(0, 0),*/ new Point(1, 0),
        new Point(-1, 1), new Point(0, 1), new Point(1, 1)
    };

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        char[][] seatingArea = lines.Select(line => line.ToCharArray()).ToArray();

        bool stabilized = false;
        while (!stabilized)
        {
            (seatingArea, stabilized) = SimulateSeatingPartTwo(seatingArea);
        }

        Console.WriteLine(CountOccupiedSeats(seatingArea));
    }

    static (char[][], bool) SimulateSeatingPartTwo(char[][] seatingArea)
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
                        if (CountVisibleOccupied(seatingArea, i, j) == 0)
                        {
                            newSeatingArea[i][j] = '#';
                            stabilized = false;
                        }
                        break;
                    case '#':
                        if (CountVisibleOccupied(seatingArea, i, j) >= 5)
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

    static int CountVisibleOccupied(char[][] seatingArea, int row, int col)
    {
        int count = 0;
        foreach (Point dir in directions)
        {
            for (int r = row + dir.y, c = col + dir.x; r >= 0 && r < seatingArea.Length && c >= 0 && c < seatingArea[0].Length; r += dir.y, c += dir.x)
            {
                if (seatingArea[r][c] == 'L')
                {
                    break;
                }
                if (seatingArea[r][c] == '#')
                {
                    count++;
                    break;
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
