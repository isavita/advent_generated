
using System;
using System.Collections.Generic;
using System.IO;

class Program
{
    static void Main()
    {
        var steps = File.ReadAllText("input.txt").Trim().Split(", ");
        int x = 0, y = 0, dir = 0;
        var visited = new HashSet<string> { "0,0" };
        int[] dx = { 0, 1, 0, -1 }, dy = { 1, 0, -1, 0 };

        foreach (var s in steps)
        {
            dir = (dir + (s[0] == 'R' ? 1 : 3)) & 3;
            for (int n = int.Parse(s.Substring(1)), i = 0; i < n; i++)
            {
                x += dx[dir]; y += dy[dir];
                string key = x + "," + y;
                if (!visited.Add(key))
                {
                    Console.WriteLine(Math.Abs(x) + Math.Abs(y));
                    return;
                }
            }
        }
    }
}
