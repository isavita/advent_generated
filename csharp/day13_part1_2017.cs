
using System;
using System.IO;
using System.Collections.Generic;

class Scanner
{
    public int Range { get; set; }
    public int Position { get; set; }
    public int Direction { get; set; }
}

class Program
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

        int severity = 0;

        for (int depth = 0; depth <= MaxDepth(firewall); depth++)
        {
            if (firewall.ContainsKey(depth))
            {
                if (firewall[depth].Position == 0)
                {
                    severity += depth * firewall[depth].Range;
                }
            }

            foreach (var scanner in firewall.Values)
            {
                MoveScanner(scanner);
            }
        }

        Console.WriteLine(severity);
    }

    static int MaxDepth(Dictionary<int, Scanner> firewall)
    {
        int max = 0;
        foreach (int depth in firewall.Keys)
        {
            if (depth > max)
            {
                max = depth;
            }
        }
        return max;
    }

    static void MoveScanner(Scanner scanner)
    {
        if (scanner.Position == 0)
        {
            scanner.Direction = 1;
        }
        else if (scanner.Position == scanner.Range - 1)
        {
            scanner.Direction = -1;
        }
        scanner.Position += scanner.Direction;
    }
}
