
using System;
using System.IO;
using System.Linq;

class Reindeer
{
    public int speed;
    public int flyTime;
    public int restTime;
    public int distance;
    public int points;
    public bool flying;
    public int timeInMode;
}

class Program
{
    static void Main()
    {
        var reindeers = ReadReindeerDetails("input.txt");
        SimulateRaceWithPoints(reindeers, 2503);
        int maxPoints = FindMaxPoints(reindeers);
        Console.WriteLine(maxPoints);
    }

    static Reindeer[] ReadReindeerDetails(string filename)
    {
        var lines = File.ReadAllLines(filename);
        return lines.Select(line =>
        {
            var parts = line.Split(' ');
            int speed = int.Parse(parts[3]);
            int flyTime = int.Parse(parts[6]);
            int restTime = int.Parse(parts[13]);
            return new Reindeer { speed = speed, flyTime = flyTime, restTime = restTime, flying = true };
        }).ToArray();
    }

    static void SimulateRaceWithPoints(Reindeer[] reindeers, int totalSeconds)
    {
        for (int i = 0; i < totalSeconds; i++)
        {
            int maxDistance = 0;
            foreach (var reindeer in reindeers)
            {
                if (reindeer.flying)
                {
                    reindeer.distance += reindeer.speed;
                }
                reindeer.timeInMode++;
                if ((reindeer.flying && reindeer.timeInMode == reindeer.flyTime) || (!reindeer.flying && reindeer.timeInMode == reindeer.restTime))
                {
                    reindeer.flying = !reindeer.flying;
                    reindeer.timeInMode = 0;
                }
                if (reindeer.distance > maxDistance)
                {
                    maxDistance = reindeer.distance;
                }
            }
            foreach (var reindeer in reindeers)
            {
                if (reindeer.distance == maxDistance)
                {
                    reindeer.points++;
                }
            }
        }
    }

    static int FindMaxPoints(Reindeer[] reindeers)
    {
        return reindeers.Max(r => r.points);
    }
}
