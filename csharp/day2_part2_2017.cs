
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var lines = File.ReadAllLines("input.txt");
        int checksum = 0, part2sum = 0;
        foreach (var line in lines)
        {
            var nums = line.Split((char[])null, StringSplitOptions.RemoveEmptyEntries)
                           .Select(int.Parse)
                           .ToArray();
            checksum += nums.Max() - nums.Min();
            foreach (var a in nums)
                foreach (var b in nums)
                    if (a != b && a % b == 0)
                        part2sum += a / b;
        }
        Console.WriteLine($"Part 1: {checksum}");
        Console.WriteLine($"Part 2: {part2sum}");
    }
}
