
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int height = lines.Length;
        int width = lines[0].Length;
        char[][] grid = new char[height][];
        int startX = -1, startY = -1;
        for (int y = 0; y < height; y++)
        {
            grid[y] = lines[y].ToCharArray();
            for (int x = 0; x < width; x++)
                if (grid[y][x] == 'S')
                {
                    startX = x;
                    startY = y;
                }
        }

        int[] dist = new int[width * height];
        for (int i = 0; i < dist.Length; i++) dist[i] = -1;

        int[] dx = { 0, -1, 0, 1 };
        int[] dy = { -1, 0, 1, 0 };
        Queue<int> q = new Queue<int>();

        int startIndex = startY * width + startX;
        dist[startIndex] = 0;
        q.Enqueue(startIndex);

        while (q.Count > 0)
        {
            int cur = q.Dequeue();
            int cx = cur % width;
            int cy = cur / width;
            int cd = dist[cur];

            for (int dir = 0; dir < 4; dir++)
            {
                int nx = cx + dx[dir];
                int ny = cy + dy[dir];
                if (nx >= 0 && nx < width && ny >= 0 && ny < height &&
                    grid[ny][nx] != '#')
                {
                    int ni = ny * width + nx;
                    if (dist[ni] == -1)
                    {
                        dist[ni] = cd + 1;
                        q.Enqueue(ni);
                    }
                }
            }
        }

        int steps = 64;
        int count = 0;
        for (int i = 0; i < dist.Length; i++)
            if (dist[i] != -1 && dist[i] <= steps && dist[i] % 2 == 0)
                count++;

        Console.WriteLine(count);
    }
}
