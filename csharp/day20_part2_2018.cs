
using System;
using System.Collections.Generic;
using System.IO;

class Point
{
    public int X { get; set; }
    public int Y { get; set; }

    public override bool Equals(object obj)
    {
        if (obj == null || GetType() != obj.GetType())
        {
            return false;
        }

        Point other = (Point)obj;
        return X == other.X && Y == other.Y;
    }

    public override int GetHashCode()
    {
        return X.GetHashCode() ^ Y.GetHashCode();
    }
}

class DoorMap : Dictionary<Point, Dictionary<Point, bool>> { }

class Program
{
    static void Main()
    {
        string regex = File.ReadAllText("input.txt");
        DoorMap dm = BuildMap(regex.Substring(1, regex.Length - 2));
        int rooms = CountRooms(dm, 1000);
        Console.WriteLine(rooms);
    }

    static DoorMap BuildMap(string regex)
    {
        DoorMap dm = new DoorMap();
        Stack<Point> stack = new Stack<Point>();
        Point cp = new Point { X = 0, Y = 0 };

        foreach (char c in regex)
        {
            if (c == '(')
            {
                stack.Push(cp);
            }
            else if (c == '|')
            {
                cp = stack.Peek();
            }
            else if (c == ')')
            {
                cp = stack.Pop();
            }
            else
            {
                Point np = Move(cp, c);
                if (!dm.ContainsKey(cp))
                {
                    dm[cp] = new Dictionary<Point, bool>();
                }
                dm[cp][np] = true;
                if (!dm.ContainsKey(np))
                {
                    dm[np] = new Dictionary<Point, bool>();
                }
                dm[np][cp] = true;
                cp = np;
            }
        }

        return dm;
    }

    static Point Move(Point p, char dir)
    {
        switch (dir)
        {
            case 'N':
                return new Point { X = p.X, Y = p.Y - 1 };
            case 'S':
                return new Point { X = p.X, Y = p.Y + 1 };
            case 'E':
                return new Point { X = p.X + 1, Y = p.Y };
            case 'W':
                return new Point { X = p.X - 1, Y = p.Y };
        }
        return p;
    }

    static int CountRooms(DoorMap dm, int minDoors)
    {
        Dictionary<Point, int> visited = new Dictionary<Point, int>();
        Queue<Point> queue = new Queue<Point>();
        Point start = new Point { X = 0, Y = 0 };
        visited[start] = 0;
        queue.Enqueue(start);
        int roomCount = 0;

        while (queue.Count > 0)
        {
            Point p = queue.Dequeue();
            foreach (Point np in dm[p].Keys)
            {
                if (!visited.ContainsKey(np))
                {
                    visited[np] = visited[p] + 1;
                    if (visited[np] >= minDoors)
                    {
                        roomCount++;
                    }
                    queue.Enqueue(np);
                }
            }
        }

        return roomCount;
    }
}
