
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    struct IpRange
    {
        public uint Start;
        public uint End;
    }

    static void Main()
    {
        List<IpRange> ipRanges = ReadIPRanges("input.txt");
        ipRanges = ipRanges.OrderBy(r => r.Start).ToList();

        uint unblockedIP = FindUnblockedIP(ipRanges);
        Console.WriteLine(unblockedIP);
    }

    static List<IpRange> ReadIPRanges(string filename)
    {
        List<IpRange> ranges = new List<IpRange>();
        using (StreamReader sr = new StreamReader(filename))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                string[] parts = line.Split('-');
                uint start = uint.Parse(parts[0]);
                uint end = uint.Parse(parts[1]);
                ranges.Add(new IpRange { Start = start, End = end });
            }
        }
        return ranges;
    }

    static uint FindUnblockedIP(List<IpRange> ranges)
    {
        uint currentIP = 0;
        foreach (var r in ranges)
        {
            if (r.Start > currentIP)
            {
                return currentIP;
            }
            if (r.End >= currentIP)
            {
                currentIP = r.End + 1;
            }
        }
        return currentIP;
    }
}
