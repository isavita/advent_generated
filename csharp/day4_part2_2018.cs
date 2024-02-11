
using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

class Program
{
    class Record
    {
        public DateTime Time { get; set; }
        public string Event { get; set; }
    }

    class Guard
    {
        public int Id { get; set; }
        public int[] Minutes { get; } = new int[60];
        public int TotalMin { get; set; }
    }

    static void Main()
    {
        List<Record> records = new List<Record>();
        Dictionary<int, Guard> guards = new Dictionary<int, Guard>();

        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                DateTime t = DateTime.ParseExact(line.Substring(1, 16), "yyyy-MM-dd HH:mm", null);
                records.Add(new Record { Time = t, Event = line.Substring(19) });
            }
        }

        records = records.OrderBy(r => r.Time).ToList();

        Guard currentGuard = null;
        int sleepStart = 0;

        foreach (Record record in records)
        {
            switch (record.Event)
            {
                case string s when s.Contains("begins shift"):
                    int id = int.Parse(s.Split(' ')[1].Substring(1));
                    if (!guards.ContainsKey(id))
                    {
                        guards[id] = new Guard { Id = id };
                    }
                    currentGuard = guards[id];
                    break;
                case string s when s.Contains("falls asleep"):
                    sleepStart = record.Time.Minute;
                    break;
                case string s when s.Contains("wakes up"):
                    for (int i = sleepStart; i < record.Time.Minute; i++)
                    {
                        currentGuard.Minutes[i]++;
                        currentGuard.TotalMin++;
                    }
                    break;
            }
        }

        Guard mostFreqGuard = null;
        int mostFreqMin = 0;

        foreach (Guard g in guards.Values)
        {
            for (int i = 0; i < 60; i++)
            {
                if (mostFreqGuard == null || g.Minutes[i] > mostFreqGuard.Minutes[mostFreqMin])
                {
                    mostFreqGuard = g;
                    mostFreqMin = i;
                }
            }
        }

        Console.WriteLine(mostFreqGuard.Id * mostFreqMin);
    }
}
