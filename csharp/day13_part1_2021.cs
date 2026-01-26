
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public class TransparentOrigami
{
    public static void Main(string[] args)
    {
        var lines = File.ReadAllLines("input.txt");
        var dotLines = lines.TakeWhile(line => !string.IsNullOrEmpty(line)).ToList();
        var foldLines = lines.Skip(dotLines.Count + 1).ToList();

        var dots = dotLines.Select(line => line.Split(',').Select(int.Parse).ToArray()).ToList();
        var folds = foldLines.Select(line => line.Split('=').ToArray()).ToList();

        var firstFold = folds[0];
        var axis = firstFold[0].Last();
        var value = int.Parse(firstFold[1]);

        for (var i = 0; i < dots.Count; i++)
        {
            if (axis == 'x' && dots[i][0] > value)
            {
                dots[i] = new[] { 2 * value - dots[i][0], dots[i][1] };
            }
            else if (axis == 'y' && dots[i][1] > value)
            {
                dots[i] = new[] { dots[i][0], 2 * value - dots[i][1] };
            }
        }

        var visibleDots = dots.Select(dot => $"{dot[0]},{dot[1]}").Distinct().Count();
        Console.WriteLine($"Visible dots after first fold: {visibleDots}");
    }
}
