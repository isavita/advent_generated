
using System;
using System.IO;
using System.Linq;

class Program
{
    static void FindCombinations(int[] containers, int target, int index, int count, ref int minCount, ref int ways)
    {
        if (target == 0)
        {
            if (minCount == 0 || count < minCount)
            {
                minCount = count;
                ways = 1;
            }
            else if (count == minCount)
            {
                ways++;
            }
            return;
        }
        if (target < 0 || index >= containers.Length)
        {
            return;
        }
        // Include current container
        FindCombinations(containers, target - containers[index], index + 1, count + 1, ref minCount, ref ways);
        // Exclude current container
        FindCombinations(containers, target, index + 1, count, ref minCount, ref ways);
    }

    static void Main()
    {
        int[] containers = File.ReadAllLines("input.txt").Select(int.Parse).ToArray();

        int minCount = 0, ways = 0;
        FindCombinations(containers, 150, 0, 0, ref minCount, ref ways);
        Console.WriteLine(ways);
    }
}
