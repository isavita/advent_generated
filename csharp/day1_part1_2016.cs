
using System;
using System.IO;

class Position
{
    public int x, y, dirIndex;
}

class Program
{
    static void Main()
    {
        string[] instructions = File.ReadAllText("input.txt").Split(", ");

        Position pos = new Position { x = 0, y = 0, dirIndex = 0 }; // Starting facing North
        int[][] directions = new int[][] { new int[] { 0, 1 }, new int[] { 1, 0 }, new int[] { 0, -1 }, new int[] { -1, 0 } }; // North, East, South, West

        foreach (string instruction in instructions)
        {
            char turn = instruction[0];
            int blocks = int.Parse(instruction.Substring(1));

            if (turn == 'R')
            {
                pos.dirIndex = (pos.dirIndex + 1) % 4;
            }
            else
            {
                pos.dirIndex = (pos.dirIndex - 1 + 4) % 4;
            }

            pos.x += directions[pos.dirIndex][0] * blocks;
            pos.y += directions[pos.dirIndex][1] * blocks;
        }

        Console.WriteLine(Math.Abs(pos.x) + Math.Abs(pos.y));
    }
}
