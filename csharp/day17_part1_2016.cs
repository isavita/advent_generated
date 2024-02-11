
using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Collections.Generic;

class Point
{
    public int x, y;
    public string path;
}

class Program
{
    static void Main()
    {
        string passcode = File.ReadAllText("input.txt").Trim();
        string path = FindShortestPath(passcode);
        Console.WriteLine(path);
    }

    static string FindShortestPath(string passcode)
    {
        var queue = new Queue<Point>();
        queue.Enqueue(new Point { x = 0, y = 0, path = "" });

        while (queue.Count > 0)
        {
            Point point = queue.Dequeue();

            if (point.x == 3 && point.y == 3)
            {
                return point.path;
            }

            foreach (var dir in GetOpenDoors(passcode, point.path))
            {
                Point nextPoint = new Point { x = point.x, y = point.y, path = point.path + dir };
                switch (dir)
                {
                    case "U":
                        nextPoint.y--;
                        break;
                    case "D":
                        nextPoint.y++;
                        break;
                    case "L":
                        nextPoint.x--;
                        break;
                    case "R":
                        nextPoint.x++;
                        break;
                }

                if (nextPoint.x >= 0 && nextPoint.x < 4 && nextPoint.y >= 0 && nextPoint.y < 4)
                {
                    queue.Enqueue(nextPoint);
                }
            }
        }

        return "No path found";
    }

    static List<string> GetOpenDoors(string passcode, string path)
    {
        string hash = Md5Hash(passcode + path);
        List<string> doors = new List<string>();
        if (hash[0] >= 'b' && hash[0] <= 'f')
        {
            doors.Add("U");
        }
        if (hash[1] >= 'b' && hash[1] <= 'f')
        {
            doors.Add("D");
        }
        if (hash[2] >= 'b' && hash[2] <= 'f')
        {
            doors.Add("L");
        }
        if (hash[3] >= 'b' && hash[3] <= 'f')
        {
            doors.Add("R");
        }
        return doors;
    }

    static string Md5Hash(string input)
    {
        using (MD5 md5 = MD5.Create())
        {
            byte[] inputBytes = Encoding.ASCII.GetBytes(input);
            byte[] hashBytes = md5.ComputeHash(inputBytes);
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < hashBytes.Length; i++)
            {
                sb.Append(hashBytes[i].ToString("x2"));
            }
            return sb.ToString();
        }
    }
}
