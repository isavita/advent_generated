
using System;
using System.IO;

public struct Sensor
{
    public int X, Y, Dist;
}

class Program
{
    static int Abs(int n) => n < 0 ? -n : n;

    static int Manhattan(int x1, int y1, int x2, int y2) => Abs(x1 - x2) + Abs(y1 - y2);

    static void Distress(Sensor[] sensors, int n, int maxCoord)
    {
        for (int x = 0; x <= maxCoord; x++)
        {
            int y = 0;
            while (y <= maxCoord)
            {
                bool detected = false;
                for (int i = 0; i < n; i++)
                {
                    if (Manhattan(sensors[i].X, sensors[i].Y, x, y) <= sensors[i].Dist)
                    {
                        detected = true;
                        int dist = sensors[i].Dist - Abs(sensors[i].X - x);
                        y = sensors[i].Y + dist + 1;
                        break;
                    }
                }
                if (!detected)
                {
                    Console.WriteLine((long)x * 4000000 + y);
                    return;
                }
            }
        }
        Console.WriteLine(-1);
    }

    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int n = lines.Length;
        Sensor[] sensors = new Sensor[n];

        for (int i = 0; i < n; i++)
        {
            string[] parts = lines[i].Split(new[] { "Sensor at x=", ", y=", ": closest beacon is at x=", ", y=" }, StringSplitOptions.None);
            sensors[i].X = int.Parse(parts[1]);
            sensors[i].Y = int.Parse(parts[2]);
            int bx = int.Parse(parts[3]);
            int by = int.Parse(parts[4]);
            sensors[i].Dist = Manhattan(sensors[i].X, sensors[i].Y, bx, by);
        }

        Distress(sensors, n, 4000000);
    }
}
