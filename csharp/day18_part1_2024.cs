
using System;
using System.IO;
using System.Linq;

namespace GridPath
{
    struct Point { public int X, Y; }
    struct QueueItem { public Point Pt; public int Steps; }

    class Program
    {
        const int SIZE = 71;
        const int MAX_QUEUE_SIZE = SIZE * SIZE;

        static void Main()
        {
            bool[,] grid = new bool[SIZE, SIZE];
            foreach (var line in File.ReadLines("input.txt")
                                      .Take(1024))
            {
                var parts = line.Split(',');
                if (parts.Length == 2 &&
                    int.TryParse(parts[0], out int x) &&
                    int.TryParse(parts[1], out int y) &&
                    x >= 0 && x < SIZE &&
                    y >= 0 && y < SIZE)
                {
                    grid[y, x] = true;
                }
            }

            bool[,] visited = new bool[SIZE, SIZE];
            var queue = new QueueItem[MAX_QUEUE_SIZE];
            int head = 0, tail = 0;

            queue[tail++] = new QueueItem { Pt = new Point { X = 0, Y = 0 }, Steps = 0 };
            visited[0, 0] = true;

            int[] dx = { 1, -1, 0, 0 };
            int[] dy = { 0, 0, 1, -1 };

            while (head < tail)
            {
                var curr = queue[head++];
                if (curr.Pt.X == SIZE - 1 && curr.Pt.Y == SIZE - 1)
                {
                    Console.WriteLine(curr.Steps);
                    return;
                }

                for (int i = 0; i < 4; i++)
                {
                    int nx = curr.Pt.X + dx[i];
                    int ny = curr.Pt.Y + dy[i];

                    if (nx >= 0 && ny >= 0 && nx < SIZE && ny < SIZE &&
                        !grid[ny, nx] && !visited[ny, nx])
                    {
                        visited[ny, nx] = true;
                        queue[tail++] = new QueueItem { Pt = new Point { X = nx, Y = ny }, Steps = curr.Steps + 1 };
                    }
                }
            }

            Console.WriteLine("No path");
        }
    }
}
