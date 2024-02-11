
using System;
using System.IO;

class Point
{
    public int x, y;

    public Point(int x, int y)
    {
        this.x = x;
        this.y = y;
    }

    public override bool Equals(object obj)
    {
        return obj is Point point &&
               x == point.x &&
               y == point.y;
    }

    public override int GetHashCode()
    {
        return HashCode.Combine(x, y);
    }
}

class Program
{
    static bool IsWall(int favoriteNumber, int x, int y)
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

    static int Bfs(Point start, Point target, int favoriteNumber)
    {
        var visited = new System.Collections.Generic.HashSet<Point>();
        var queue = new System.Collections.Generic.Queue<Point>();
        queue.Enqueue(start);
        int steps = 0;

        while (queue.Count > 0)
        {
            int size = queue.Count;
            for (int i = 0; i < size; i++)
            {
                var point = queue.Dequeue();
                if (point.Equals(target))
                {
                    return steps;
                }

                Point[] deltas = { new Point(1, 0), new Point(-1, 0), new Point(0, 1), new Point(0, -1) };
                foreach (var delta in deltas)
                {
                    var next = new Point(point.x + delta.x, point.y + delta.y);
                    if (next.x >= 0 && next.y >= 0 && !IsWall(favoriteNumber, next.x, next.y) && !visited.Contains(next))
                    {
                        visited.Add(next);
                        queue.Enqueue(next);
                    }
                }
            }
            steps++;
        }

        return -1;
    }

    static void Main()
    {
        string input = File.ReadAllText("input.txt").Trim();
        int favoriteNumber = int.Parse(input);
        Point start = new Point(1, 1);
        Point target = new Point(31, 39);
        int steps = Bfs(start, target, favoriteNumber);
        Console.WriteLine(steps);
    }
}
