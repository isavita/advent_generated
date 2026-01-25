
using System;
using System.IO;
using System.Linq;

public class Program
{
    private const int Width = 7;
    private const int NumRocks = 2022;
    private const int NumShapes = 5;
    private const int MaxRockPoints = 5;
    private const int ChamberHeight = 4000;

    private struct Point
    {
        public int X;
        public int Y;
    }

    private static readonly Point[][] RockShapes = new[]
    {
        new[] { new Point { X = 0, Y = 0 }, new Point { X = 1, Y = 0 }, new Point { X = 2, Y = 0 }, new Point { X = 3, Y = 0 } },
        new[] { new Point { X = 1, Y = 0 }, new Point { X = 0, Y = 1 }, new Point { X = 1, Y = 1 }, new Point { X = 2, Y = 1 }, new Point { X = 1, Y = 2 } },
        new[] { new Point { X = 0, Y = 0 }, new Point { X = 1, Y = 0 }, new Point { X = 2, Y = 0 }, new Point { X = 2, Y = 1 }, new Point { X = 2, Y = 2 } },
        new[] { new Point { X = 0, Y = 0 }, new Point { X = 0, Y = 1 }, new Point { X = 0, Y = 2 }, new Point { X = 0, Y = 3 } },
        new[] { new Point { X = 0, Y = 0 }, new Point { X = 1, Y = 0 }, new Point { X = 0, Y = 1 }, new Point { X = 1, Y = 1 } }
    };

    private static readonly int[] RockSizes = new[] { 4, 5, 5, 4, 4 };

    private static bool[,] Chamber = new bool[ChamberHeight, Width];
    private static int highestY = 0;

    public static void Main(string[] args)
    {
        var jetPattern = File.ReadAllText("input.txt").Trim();
        var jetIndex = 0;

        var currentRock = new Point[MaxRockPoints];

        for (var rockNum = 0; rockNum < NumRocks; rockNum++)
        {
            var shapeIndex = rockNum % NumShapes;
            var currentRockSize = RockSizes[shapeIndex];

            var startX = 2;
            var startY = highestY + 4;

            for (var i = 0; i < currentRockSize; i++)
            {
                currentRock[i].X = startX + RockShapes[shapeIndex][i].X;
                currentRock[i].Y = startY + RockShapes[shapeIndex][i].Y;
            }

            while (true)
            {
                var jet = jetPattern[jetIndex % jetPattern.Length];
                jetIndex++;
                var dxJet = jet == '>' ? 1 : -1;

                if (CanMove(currentRock, currentRockSize, dxJet, 0))
                {
                    MoveRock(currentRock, currentRockSize, dxJet, 0);
                }

                if (CanMove(currentRock, currentRockSize, 0, -1))
                {
                    MoveRock(currentRock, currentRockSize, 0, -1);
                }
                else
                {
                    SettleRock(currentRock, currentRockSize);
                    break;
                }
            }

            if (highestY >= ChamberHeight - 10)
            {
                Console.Error.WriteLine("Warning: Chamber height approaching limit.");
            }
        }

        Console.WriteLine(highestY);
    }

    private static bool IsValid(int x, int y)
    {
        return x >= 0 && x < Width && y > 0 && y < ChamberHeight && !Chamber[y, x];
    }

    private static bool CanMove(Point[] rock, int size, int dx, int dy)
    {
        for (var i = 0; i < size; i++)
        {
            if (!IsValid(rock[i].X + dx, rock[i].Y + dy))
            {
                return false;
            }
        }
        return true;
    }

    private static void MoveRock(Point[] rock, int size, int dx, int dy)
    {
        for (var i = 0; i < size; i++)
        {
            rock[i].X += dx;
            rock[i].Y += dy;
        }
    }

    private static void SettleRock(Point[] rock, int size)
    {
        for (var i = 0; i < size; i++)
        {
            Chamber[rock[i].Y, rock[i].X] = true;
            if (rock[i].Y > highestY)
            {
                highestY = rock[i].Y;
            }
        }
    }
}
