
using System;
using System.IO;
using System.Linq;

class Reindeer
{
    public int speed;
    public int flyTime;
    public int restTime;
    public int distance;
    public bool flying = true;
    public int timeInMode = 0;
}

class Program
{
    static void Main()
    {
        var reindeers = ReadReindeerDetails("input.txt");
        if (reindeers == null)
        {
            Console.WriteLine("Error reading input");
            return;
        }

        SimulateRace(reindeers, 2503);
        int maxDistance = FindMaxDistance(reindeers);
        Console.WriteLine(maxDistance);
    }

    static Reindeer[] ReadReindeerDetails(string filename)
    {
        try
        {
            string[] lines = File.ReadAllLines(filename);
            return lines.Select(line =>
            {
                var parts = line.Split(' ');
                return new Reindeer
                {
                    speed = int.Parse(parts[3]),
                    flyTime = int.Parse(parts[6]),
                    restTime = int.Parse(parts[13])
                };
            }).ToArray();
        }
        catch (Exception)
        {
            return null;
        }
    }

    static void SimulateRace(Reindeer[] reindeers, int totalSeconds)
    {
        for (int i = 0; i < totalSeconds; i++)
        {
            foreach (var reindeer in reindeers)
            {
                if (reindeer.flying)
                {
                    reindeer.distance += reindeer.speed;
                    reindeer.timeInMode++;
                    if (reindeer.timeInMode == reindeer.flyTime)
                    {
                        reindeer.flying = false;
                        reindeer.timeInMode = 0;
                    }
                }
                else
                {
                    reindeer.timeInMode++;
                    if (reindeer.timeInMode == reindeer.restTime)
                    {
                        reindeer.flying = true;
                        reindeer.timeInMode = 0;
                    }
                }
            }
        }
    }

    static int FindMaxDistance(Reindeer[] reindeers)
    {
        int maxDistance = 0;
        foreach (var reindeer in reindeers)
        {
            if (reindeer.distance > maxDistance)
            {
                maxDistance = reindeer.distance;
            }
        }
        return maxDistance;
    }
}
