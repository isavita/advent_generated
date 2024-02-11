
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Record
{
    public DateTime Timestamp { get; set; }
    public string Action { get; set; }
    public int GuardID { get; set; }
}

class Program
{
    static void Main()
    {
        List<Record> records = ReadAndParseInput("input.txt");
        records = records.OrderBy(r => r.Timestamp).ToList();

        Dictionary<int, int[]> guardSleepMinutes = new Dictionary<int, int[]>();
        int currentGuardID = 0;
        DateTime sleepStart = DateTime.MinValue;

        foreach (var record in records)
        {
            switch (record.Action)
            {
                case "begins shift":
                    currentGuardID = record.GuardID;
                    break;
                case "falls asleep":
                    sleepStart = record.Timestamp;
                    break;
                case "wakes up":
                    if (!guardSleepMinutes.ContainsKey(currentGuardID))
                    {
                        guardSleepMinutes[currentGuardID] = new int[60];
                    }
                    for (int i = sleepStart.Minute; i < record.Timestamp.Minute; i++)
                    {
                        guardSleepMinutes[currentGuardID][i]++;
                    }
                    break;
            }
        }

        int maxSleep = 0;
        int sleepiestGuard = 0;
        foreach (var kvp in guardSleepMinutes)
        {
            int totalSleep = kvp.Value.Sum();
            if (totalSleep > maxSleep)
            {
                maxSleep = totalSleep;
                sleepiestGuard = kvp.Key;
            }
        }

        int maxMinute = 0;
        int maxMinuteCount = 0;
        for (int i = 0; i < 60; i++)
        {
            if (guardSleepMinutes[sleepiestGuard][i] > maxMinuteCount)
            {
                maxMinuteCount = guardSleepMinutes[sleepiestGuard][i];
                maxMinute = i;
            }
        }

        Console.WriteLine(sleepiestGuard * maxMinute);
    }

    static List<Record> ReadAndParseInput(string filename)
    {
        string[] lines = File.ReadAllLines(filename);
        List<Record> records = new List<Record>();
        string layout = "yyyy-MM-dd HH:mm";

        foreach (var line in lines)
        {
            string[] parts = line.Split(new[] { "] " }, StringSplitOptions.None);
            string timePart = parts[0].Substring(1);
            string actionPart = parts[1];

            DateTime ts = DateTime.ParseExact(timePart, layout, null);

            int guardID = -1;
            if (actionPart.Contains("Guard"))
            {
                int startIndex = actionPart.IndexOf("#") + 1;
                int endIndex = actionPart.IndexOf(" ", startIndex);
                guardID = int.Parse(actionPart.Substring(startIndex, endIndex - startIndex));
                actionPart = "begins shift";
            }
            else if (actionPart.Contains("falls asleep"))
            {
                actionPart = "falls asleep";
            }
            else if (actionPart.Contains("wakes up"))
            {
                actionPart = "wakes up";
            }

            records.Add(new Record { Timestamp = ts, Action = actionPart, GuardID = guardID });
        }

        return records;
    }
}
