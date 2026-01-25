
using System;
using System.IO;

class Program
{
    const int MAX_GRID_SIZE = 1000;

    static void Main(string[] args)
    {
        string[] lines = File.ReadAllLines("input.txt");
        char[,] grid = new char[lines.Length, lines[0].Length];

        for (int i = 0; i < lines.Length; i++)
        {
            for (int j = 0; j < lines[i].Length; j++)
            {
                grid[i, j] = lines[i][j];
            }
        }

        int x = 0, y = 0, dirIdx = 0;
        int[,] dirs = { { 0, -1 }, { 1, 0 }, { 0, 1 }, { -1, 0 } };

        for (int i = 0; i < grid.GetLength(0); i++)
        {
            for (int j = 0; j < grid.GetLength(1); j++)
            {
                switch (grid[i, j])
                {
                    case '^': x = j; y = i; dirIdx = 0; goto Found;
                    case '>': x = j; y = i; dirIdx = 1; goto Found;
                    case 'v': x = j; y = i; dirIdx = 2; goto Found;
                    case '<': x = j; y = i; dirIdx = 3; goto Found;
                }
            }
        }

    Found:
        int dirX = dirs[dirIdx, 0];
        int dirY = dirs[dirIdx, 1];

        bool[,] visited = new bool[grid.GetLength(0), grid.GetLength(1)];
        int count = 0;
        visited[y, x] = true;
        count++;

        while (true)
        {
            int nx = x + dirX;
            int ny = y + dirY;

            if (nx < 0 || nx >= grid.GetLength(1) || ny < 0 || ny >= grid.GetLength(0))
                break;

            if (grid[ny, nx] == '#')
            {
                dirIdx = (dirIdx + 1) % 4;
                dirX = dirs[dirIdx, 0];
                dirY = dirs[dirIdx, 1];
                continue;
            }
            x = nx;
            y = ny;
            if (!visited[y, x])
            {
                visited[y, x] = true;
                count++;
            }
        }

        Console.WriteLine(count);
    }
}
