
using System;
using System.IO;

class Program
{
    static void Main()
    {
        StreamReader file = new StreamReader("input.txt");
        int horizontalPosition = 0;
        int depth = 0;
        int aim = 0;

        string line;
        while ((line = file.ReadLine()) != null)
        {
            string[] command = line.Split(' ');
            string direction = command[0];
            int units = int.Parse(command[1]);

            switch (direction)
            {
                case "forward":
                    horizontalPosition += units;
                    depth += aim * units;
                    break;
                case "down":
                    aim += units;
                    break;
                case "up":
                    aim -= units;
                    break;
            }
        }

        int product = horizontalPosition * depth;
        Console.WriteLine(product);
    }
}
