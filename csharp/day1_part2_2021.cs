
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        var vals = lines.Where(line => !string.IsNullOrEmpty(line)).Select(int.Parse).ToList();

        int prevSum = vals[0] + vals[1] + vals[2];
        int count = 0;
        for (int i = 3; i < vals.Count; i++)
        {
            int currSum = vals[i - 2] + vals[i - 1] + vals[i];
            if (currSum > prevSum)
            {
                count++;
            }
            prevSum = currSum;
        }

        Console.WriteLine(count);
    }
}
