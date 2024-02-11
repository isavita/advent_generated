
using System;
using System.IO;
using System.Collections.Generic;

class Scanner
{
    public int Range { get; set; }
    public int Position { get; set; }
    public int Direction { get; set; }
}

class Solution
{
    static void Main()
    {
        Dictionary<int, Scanner> firewall = new Dictionary<int, Scanner>();
        using (StreamReader sr = new StreamReader("input.txt"))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                string[] fields = line.Split(": ");
                int depth = int.Parse(fields[0]);
                int rng = int.Parse(fields[1]);
                firewall[depth] = new Scanner { Range = rng, Position = 0, Direction = 1 };
            }
        }

        int delay = 0;
        while (!PassThrough(firewall, delay))
        {
            delay++;
        }

        Console.WriteLine(delay);
    }

    static bool PassThrough(Dictionary<int, Scanner> firewall, int delay)
    {
        foreach (var kvp in firewall)
        {
            int depth = kvp.Key;
            Scanner scanner = kvp.Value;
            if ((depth + delay) % (2 * (scanner.Range - 1)) == 0)
            {
                return false;
            }
        }
        return true;
    }
}
