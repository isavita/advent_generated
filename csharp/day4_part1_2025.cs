
using System;
using System.IO;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int rows = lines.Length;
        if (rows == 0) return;
        int cols = lines[0].Length;
        int[,] dx = { { -1, -1, -1, 0, 0, 1, 1, 1 }, { -1, 0, 1, -1, 1, -1, 0, 1 } };
        int acc = 0;
        for (int y = 0; y < rows; y++)
        {
            for (int x = 0; x < cols; x++)
            {
                if (lines[y][x] != '@') continue;
                int cnt = 0;
                for (int d = 0; d < 8; d++)
                {
                    int nx = x + dx[0, d], ny = y + dx[1, d];
                    if (nx >= 0 && nx < cols && ny >= 0 && ny < rows && lines[ny][nx] == '@') cnt++;
                }
                if (cnt < 4) acc++;
            }
        }
        Console.WriteLine(acc);
    }
}
