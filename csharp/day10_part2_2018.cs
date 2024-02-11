
using System;
using System.IO;
using System.Text.RegularExpressions;

class Star
{
    public int x;
    public int y;
    public int vX;
    public int vY;
    public Star next;
}

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        Star head = new Star();
        Star tail = head;
        Regex re = new Regex(@"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>");
        
        foreach (string line in lines)
        {
            Match match = re.Match(line);
            if (match.Success)
            {
                Star star = new Star
                {
                    x = int.Parse(match.Groups[1].Value),
                    y = int.Parse(match.Groups[2].Value),
                    vX = int.Parse(match.Groups[3].Value),
                    vY = int.Parse(match.Groups[4].Value)
                };
                tail.next = star;
                tail = star;
            }
        }

        int smallestT = 0;
        int smallestArea = int.MaxValue / 2;

        for (int t = 1; t < 100000; t++)
        {
            int maxX = 0;
            int maxY = 0;
            int minX = 0;
            int minY = 0;

            for (Star temp = head.next; temp.next != null; temp = temp.next)
            {
                int x = temp.x + temp.vX * t;
                if (maxX < x)
                {
                    maxX = x;
                }
                else if (minX > x)
                {
                    minX = x;
                }
                int y = temp.y + temp.vY * t;
                if (maxY < y)
                {
                    maxY = y;
                }
                else if (minY > y)
                {
                    minY = y;
                }
            }

            int lenX = maxX - minY + 1;
            int lenY = maxY - minY + 1;
            int area = lenX + lenY;

            if (smallestArea > area)
            {
                smallestArea = area;
                smallestT = t;
            }
        }

        Console.WriteLine(smallestT);
    }
}
