
using System;
using System.IO;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        StreamReader file = new StreamReader("input.txt");
        Dictionary<Tuple<int, int>, bool> points = new Dictionary<Tuple<int, int>, bool>();
        List<Tuple<int, int>> folds = new List<Tuple<int, int>>();
        bool readingPoints = true;

        string line;
        while ((line = file.ReadLine()) != null)
        {
            if (line == "")
            {
                readingPoints = false;
                continue;
            }

            if (readingPoints)
            {
                string[] parts = line.Split(',');
                int x = int.Parse(parts[0]);
                int y = int.Parse(parts[1]);
                Tuple<int, int> point = new Tuple<int, int>(x, y);
                if (!points.ContainsKey(point))
                {
                    points.Add(point, true);
                }
            }
            else
            {
                string[] parts = line.Split('=');
                int val = int.Parse(parts[1]);
                if (parts[0].Contains("x"))
                {
                    folds.Add(new Tuple<int, int>(val, 0));
                }
                else
                {
                    folds.Add(new Tuple<int, int>(0, val));
                }
            }
        }

        foreach (var fold in folds)
        {
            Dictionary<Tuple<int, int>, bool> newPoints = new Dictionary<Tuple<int, int>, bool>();
            foreach (var point in points.Keys)
            {
                int newX = point.Item1;
                int newY = point.Item2;
                if (fold.Item1 != 0 && point.Item1 > fold.Item1)
                {
                    newX = fold.Item1 - (point.Item1 - fold.Item1);
                }
                else if (fold.Item2 != 0 && point.Item2 > fold.Item2)
                {
                    newY = fold.Item2 - (point.Item2 - fold.Item2);
                }
                Tuple<int, int> newPoint = new Tuple<int, int>(newX, newY);
                if (!newPoints.ContainsKey(newPoint))
                {
                    newPoints.Add(newPoint, true);
                }
            }
            points = newPoints;
            Console.WriteLine("Number of dots visible after first fold: " + points.Count);
        }

        int maxX = 0;
        int maxY = 0;
        foreach (var point in points.Keys)
        {
            if (point.Item1 > maxX)
            {
                maxX = point.Item1;
            }
            if (point.Item2 > maxY)
            {
                maxY = point.Item2;
            }
        }

        char[][] grid = new char[maxY + 1][];
        for (int i = 0; i < maxY + 1; i++)
        {
            grid[i] = new char[maxX + 1];
            for (int j = 0; j < maxX + 1; j++)
            {
                grid[i][j] = ' ';
            }
        }

        foreach (var point in points.Keys)
        {
            grid[point.Item2][point.Item1] = '#';
        }

        foreach (var row in grid)
        {
            Console.WriteLine(new string(row));
        }
    }
}
