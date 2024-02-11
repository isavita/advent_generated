
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        int earliestDeparture = int.Parse(lines[0]);
        string[] busIDs = lines[1].Split(',');

        int earliestBusID = 0;
        int minWaitTime = earliestDeparture;

        foreach (var id in busIDs)
        {
            if (id == "x")
            {
                continue;
            }
            int busID = int.Parse(id);
            int waitTime = busID - (earliestDeparture % busID);
            if (waitTime < minWaitTime)
            {
                minWaitTime = waitTime;
                earliestBusID = busID;
            }
        }

        Console.WriteLine(earliestBusID * minWaitTime);
    }
}
