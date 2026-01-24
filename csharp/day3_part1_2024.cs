
using System;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        var input = File.ReadAllText("input.txt");
        long sum = 0;
        foreach (Match m in Regex.Matches(input, @"mul\((\d{1,3}),(\d{1,3})\)"))
            sum += int.Parse(m.Groups[1].Value) * int.Parse(m.Groups[2].Value);
        Console.WriteLine(sum);
    }
}
