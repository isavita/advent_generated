
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        string[] frequencyChanges = File.ReadAllLines("input.txt");
        var frequencies = new System.Collections.Generic.HashSet<int>();
        int currentFrequency = 0;
        frequencies.Add(currentFrequency);

        while (true)
        {
            foreach (var change in frequencyChanges)
            {
                if (!int.TryParse(change, out int frequencyDelta))
                {
                    Console.WriteLine("Error converting string to int");
                    return;
                }
                currentFrequency += frequencyDelta;
                if (frequencies.Contains(currentFrequency))
                {
                    Console.WriteLine(currentFrequency);
                    return;
                }
                frequencies.Add(currentFrequency);
            }
        }
    }
}
