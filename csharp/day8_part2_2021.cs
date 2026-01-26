
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        var input = File.ReadAllText("input.txt");
        var ans = Solve(input);
        Console.WriteLine(ans);
    }

    static int Solve(string input)
    {
        var lines = input.Split('\n', StringSplitOptions.RemoveEmptyEntries);
        var total = 0;
        foreach (var line in lines)
        {
            var parts = Regex.Matches(line, "[a-g]+")
                             .Select(m => Alphabetize(m.Value))
                             .ToArray();
            var digits = new string[10];
            var working = parts.Take(10).ToArray();

            var one = working.First(p => p.Length == 2);
            var four = working.First(p => p.Length == 4);
            var seven = working.First(p => p.Length == 3);
            var eight = working.First(p => p.Length == 7);
            digits[1] = one; digits[4] = four; digits[7] = seven; digits[8] = eight;

            var filtered = working.Except(new[] { one, four, seven, eight }).ToArray();

            var zeroThreeNine = filtered.Where(p => Overlap(p, one)).ToArray();
            var three = zeroThreeNine.First(p => p.Length == 5);
            var nine = zeroThreeNine.First(p => p != three && Overlap(p, four));
            var zero = zeroThreeNine.First(p => p != three && p != nine);
            digits[3] = three; digits[9] = nine; digits[0] = zero;

            var remaining = filtered.Except(zeroThreeNine).ToArray();
            var six = remaining.First(p => p.Length == 6);
            var five = remaining.First(p => p != six && Overlap(nine, p));
            var two = remaining.First(p => p != six && p != five);
            digits[6] = six; digits[5] = five; digits[2] = two;

            var output = parts.Skip(10).Take(4);
            var value = 0;
            foreach (var o in output)
                value = value * 10 + Array.IndexOf(digits, o);
            total += value;
        }
        return total;
    }

    static string Alphabetize(string s) => new string(s.OrderBy(c => c).ToArray());
    static bool Overlap(string larger, string smaller) => smaller.All(larger.Contains);
}
