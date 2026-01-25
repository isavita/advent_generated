
using System;
using System.IO;
using System.Linq;

public class Program
{
    public static void Main()
    {
        var seatIds = File.ReadAllLines("input.txt")
            .Select(CalculateSeatId)
            .OrderBy(id => id)
            .ToArray();

        Console.WriteLine(seatIds.Max());

        for (int i = 1; i < seatIds.Length - 1; i++)
        {
            if (seatIds[i + 1] - seatIds[i] == 2)
            {
                Console.WriteLine(seatIds[i] + 1);
                break;
            }
        }
    }

    public static int CalculateSeatId(string boardingPass)
    {
        int row = Convert.ToInt32(boardingPass.Substring(0, 7).Replace("F", "0").Replace("B", "1"), 2);
        int col = Convert.ToInt32(boardingPass.Substring(7, 3).Replace("L", "0").Replace("R", "1"), 2);
        return row * 8 + col;
    }
}
