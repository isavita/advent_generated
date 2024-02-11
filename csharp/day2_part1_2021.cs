
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");

        int horizontalPosition = 0;
        int depth = 0;

        foreach (string line in lines)
        {
            string[] command = line.Split(' ');
            string direction = command[0];
            int units = int.Parse(command[1]);

            switch (direction)
            {
                case "forward":
                    horizontalPosition += units;
                    break;
                case "down":
                    depth += units;
                    break;
                case "up":
                    depth -= units;
                    break;
            }
        }

        int product = horizontalPosition * depth;
        Console.WriteLine(product);
    }
}
