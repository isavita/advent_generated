
using System;
using System.IO;
using System.Linq;

class Program
{
    static int CountCombinations(int[] containers, int target, int index)
    {
        if (target == 0)
        {
            return 1;
        }
        if (target < 0 || index >= containers.Length)
        {
            return 0;
        }
        return CountCombinations(containers, target - containers[index], index + 1) +
               CountCombinations(containers, target, index + 1);
    }

    static void Main()
    {
        int[] containers = File.ReadAllLines("input.txt").Select(int.Parse).ToArray();

        Console.WriteLine(CountCombinations(containers, 150, 0));
    }
}
