
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        Console.WriteLine(Solve(input));
    }

    static int Solve(string input)
    {
        var matrix = ParseInput(input);
        int originCol = 0;
        for (int i = 0; i < matrix[0].Length; i++)
        {
            if (matrix[0][i] == "+")
            {
                originCol = i;
            }
            matrix[matrix.Length - 1][i] = "#";
        }

        int ans = 0;
        while (!DropSand(matrix, originCol))
        {
            ans++;
            if (matrix[0][originCol] == "o")
            {
                break;
            }
        }

        return ans;
    }

    static string[][] ParseInput(string input)
    {
        var coordSets = new int[][][] { };
        int lowestCol = int.MaxValue;
        int highestRow = 0;

        foreach (var line in input.Split('\n'))
        {
            var rawCoords = line.Split(" -> ");
            var coords = new int[][] { };
            foreach (var rawCoord in rawCoords)
            {
                var rawNums = rawCoord.Split(",");
                int col = int.Parse(rawNums[0]);
                int row = int.Parse(rawNums[1]);
                coords = coords.Append(new int[] { col, row }).ToArray();

                lowestCol = Math.Min(lowestCol, col);
                highestRow = Math.Max(highestRow, row);
            }
            coordSets = coordSets.Append(coords).ToArray();
        }

        int ExtraLeftSpace = 200;
        int highestCol = 0;

        for (int s = 0; s < coordSets.Length; s++)
        {
            for (int i = 0; i < coordSets[s].Length; i++)
            {
                coordSets[s][i][0] -= lowestCol - ExtraLeftSpace;
                highestCol = Math.Max(highestCol, coordSets[s][i][0]);
            }
        }

        var matrix = new string[highestRow + 3][];
        for (int r = 0; r < matrix.Length; r++)
        {
            matrix[r] = new string[highestCol + ExtraLeftSpace * 2];
        }

        foreach (var set in coordSets)
        {
            for (int i = 1; i < set.Length; i++)
            {
                int[] cols = { set[i - 1][0], set[i][0] };
                int[] rows = { set[i - 1][1], set[i][1] };

                Array.Sort(cols);
                Array.Sort(rows);

                if (cols[0] == cols[1])
                {
                    for (int r = rows[0]; r <= rows[1]; r++)
                    {
                        matrix[r][cols[0]] = "#";
                    }
                }
                else if (rows[0] == rows[1])
                {
                    for (int c = cols[0]; c <= cols[1]; c++)
                    {
                        matrix[rows[0]][c] = "#";
                    }
                }
            }
        }

        int originCol = 500 - lowestCol + ExtraLeftSpace;
        matrix[0][originCol] = "+";

        for (int i = 0; i < matrix.Length; i++)
        {
            for (int j = 0; j < matrix[i].Length; j++)
            {
                if (matrix[i][j] == null)
                {
                    matrix[i][j] = ".";
                }
            }
        }

        return matrix;
    }

    static void PrintMatrix(string[][] matrix)
    {
        foreach (var row in matrix)
        {
            Console.WriteLine(string.Join(" ", row));
        }
    }

    static bool DropSand(string[][] matrix, int originCol)
    {
        int r = 0, c = originCol;

        while (r < matrix.Length - 1)
        {
            string below = matrix[r + 1][c];
            string diagonallyLeft = matrix[r + 1][c - 1];
            string diagonallyRight = matrix[r + 1][c + 1];
            if (below == ".")
            {
                r++;
            }
            else if (diagonallyLeft == ".")
            {
                r++;
                c--;
            }
            else if (diagonallyRight == ".")
            {
                r++;
                c++;
            }
            else
            {
                matrix[r][c] = "o";
                return false;
            }
        }

        return true;
    }
}
