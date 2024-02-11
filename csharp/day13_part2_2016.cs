
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    const int favoriteNumber = 1362;

    struct Point
    {
        public int x, y;
    }

    static bool IsWall(int x, int y)
    {
        int num = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
        int bits = 0;
        while (num > 0)
        {
            if (num % 2 == 1)
            {
                bits++;
            }
            num /= 2;
        }
        return bits % 2 != 0;
    }

    static int BfsMaxSteps(Point start, int maxSteps)
    {
        Dictionary<Point, bool> visited = new Dictionary<Point, bool>();
        Queue<Point> queue = new Queue<Point>();
        queue.Enqueue(start);
        visited[start] = true;
        int steps = 0;

        while (queue.Count > 0 && steps < maxSteps)
        {
            int size = queue.Count;
            for (int i = 0; i < size; i++)
            {
                Point point = queue.Dequeue();

                Point[] deltas = { new Point { x = 1, y = 0 }, new Point { x = -1, y = 0 }, new Point { x = 0, y = 1 }, new Point { x = 0, y = -1 } };
                foreach (var delta in deltas)
                {
                    Point next = new Point { x = point.x + delta.x, y = point.y + delta.y };
                    if (next.x >= 0 && next.y >= 0 && !IsWall(next.x, next.y) && !visited.ContainsKey(next))
                    {
                        visited[next] = true;
                        queue.Enqueue(next);
                    }
                }
            }
            steps++;
        }

        return visited.Count;
    }

    static void Main()
    {
        Point start = new Point { x = 1, y = 1 };
        int reachableLocations = BfsMaxSteps(start, 50);
        Console.WriteLine(reachableLocations);
    }
}
