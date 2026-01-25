
using System;
using System.IO;
using System.Linq;

public struct Point
{
    public int X { get; set; }
    public int Y { get; set; }
}

public struct Sensor
{
    public Point Position { get; set; }
    public Point Beacon { get; set; }
    public int Distance { get; set; }
}

class Program
{
    static int ManhattanDistance(Point p, Point q)
    {
        return Math.Abs(p.X - q.X) + Math.Abs(p.Y - q.Y);
    }

    static int Impossible(Sensor[] sensors, int y)
    {
        int minX = int.MaxValue;
        int maxX = int.MinValue;

        foreach (var sensor in sensors)
        {
            int dist = sensor.Distance - Math.Abs(sensor.Position.Y - y);
            if (dist < 0) continue;
            int startX = sensor.Position.X - dist;
            int endX = sensor.Position.X + dist;
            if (startX < minX) minX = startX;
            if (endX > maxX) maxX = endX;
        }

        if (minX == int.MaxValue || maxX == int.MinValue) return 0;

        bool[] covered = new bool[maxX - minX + 1];

        foreach (var sensor in sensors)
        {
            int dist = sensor.Distance - Math.Abs(sensor.Position.Y - y);
            if (dist < 0) continue;
            int startX = sensor.Position.X - dist;
            int endX = sensor.Position.X + dist;

            for (int x = startX; x <= endX; x++)
            {
                covered[x - minX] = true;
            }
        }

        foreach (var sensor in sensors)
        {
            if (sensor.Beacon.Y == y)
            {
                if (sensor.Beacon.X >= minX && sensor.Beacon.X <= maxX)
                    covered[sensor.Beacon.X - minX] = false;
            }
        }

        return covered.Count(b => b);
    }

    static void Main(string[] args)
    {
        string[] lines = File.ReadAllLines("input.txt");
        Sensor[] sensors = new Sensor[lines.Length];

        for (int i = 0; i < lines.Length; i++)
        {
            string[] parts = lines[i].Split(new[] { '=', ',', ':', ' ' }, StringSplitOptions.RemoveEmptyEntries);
            sensors[i].Position = new Point { X = int.Parse(parts[3]), Y = int.Parse(parts[5]) };
            sensors[i].Beacon = new Point { X = int.Parse(parts[11]), Y = int.Parse(parts[13]) };
            sensors[i].Distance = ManhattanDistance(sensors[i].Position, sensors[i].Beacon);
        }

        Console.WriteLine(Impossible(sensors, 2000000));
    }
}
