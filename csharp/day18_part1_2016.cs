
using System;
using System.IO;

class Program
{
    const int TotalRows = 40;

    static void Main()
    {
        string firstRow = ReadFirstRow("input.txt");
        int safeTilesCount = CountSafeTiles(firstRow, TotalRows);
        Console.WriteLine(safeTilesCount);
    }

    static string ReadFirstRow(string filename)
    {
        using (StreamReader sr = new StreamReader(filename))
        {
            return sr.ReadLine();
        }
    }

    static int CountSafeTiles(string firstRow, int totalRows)
    {
        string currentRow = firstRow;
        int safeCount = CountChar(currentRow, '.');

        for (int i = 1; i < totalRows; i++)
        {
            string nextRow = "";
            for (int j = 0; j < currentRow.Length; j++)
            {
                if (IsTrap(j - 1, j, j + 1, currentRow))
                {
                    nextRow += "^";
                }
                else
                {
                    nextRow += ".";
                    safeCount++;
                }
            }
            currentRow = nextRow;
        }
        return safeCount;
    }

    static bool IsTrap(int left, int center, int right, string row)
    {
        char l = SafeIfOutOfBounds(left, row);
        char c = row[center];
        char r = SafeIfOutOfBounds(right, row);

        return (l == '^' && c == '^' && r == '.') ||
               (c == '^' && r == '^' && l == '.') ||
               (l == '^' && c == '.' && r == '.') ||
               (r == '^' && c == '.' && l == '.');
    }

    static char SafeIfOutOfBounds(int index, string row)
    {
        if (index < 0 || index >= row.Length)
        {
            return '.';
        }
        return row[index];
    }

    static int CountChar(string str, char character)
    {
        int count = 0;
        foreach (char c in str)
        {
            if (c == character)
            {
                count++;
            }
        }
        return count;
    }
}
