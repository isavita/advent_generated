using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var height = lines.Length;
        var width = lines[0].Length;
        var map = new char[height][];
        for (int i = 0; i < height; i++) map[i] = lines[i].ToCharArray();

        int sx = 0, sy = 0, ex = 0, ey = 0;
        for (int y = 0; y < height; y++)
            for (int x = 0; x < width; x++)
                if (map[y][x] == 'S') { sx = x; sy = y; }
                else if (map[y][x] == 'E') { ex = x; ey = y; }

        var visited = new bool[height, width];
        var q = new Queue<(int x, int y, int d, char e)>();
        q.Enqueue((sx, sy, 0, 'a'));
        visited[sy, sx] = true;

        int[] dx = { 0, 0, 1, -1 };
        int[] dy = { 1, -1, 0, 0 };

        while (q.Count > 0)
        {
            var (x, y, d, e) = q.Dequeue();
            if (x == ex && y == ey) { Console.WriteLine(d); return; }

            for (int i = 0; i < 4; i++)
            {
                int nx = x + dx[i], ny = y + dy[i];
                if (nx < 0 || nx >= width || ny < 0 || ny >= height) continue;
                if (visited[ny, nx]) continue;
                char ne = map[ny][nx];
                int from = e - 'a';
                int to = ne == 'E' ? 25 : ne - 'a';
                if (to - from <= 1)
                {
                    visited[ny, nx] = true;
                    q.Enqueue((nx, ny, d + 1, ne));
                }
            }
        }
    }
}