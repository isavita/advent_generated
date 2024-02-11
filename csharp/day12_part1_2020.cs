
using System;
using System.IO;

class Ship
{
    public int x, y, facing;

    public void ProcessInstruction(char action, int value)
    {
        switch (action)
        {
            case 'N':
                y += value;
                break;
            case 'S':
                y -= value;
                break;
            case 'E':
                x += value;
                break;
            case 'W':
                x -= value;
                break;
            case 'L':
                facing = (facing - value + 360) % 360;
                break;
            case 'R':
                facing = (facing + value) % 360;
                break;
            case 'F':
                switch (facing)
                {
                    case 0:
                        x += value;
                        break;
                    case 90:
                        y -= value;
                        break;
                    case 180:
                        x -= value;
                        break;
                    case 270:
                        y += value;
                        break;
                }
                break;
        }
    }
}

class Program
{
    static void Main()
    {
        using (StreamReader sr = new StreamReader("input.txt"))
        {
            Ship ship = new Ship { x = 0, y = 0, facing = 0 };
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                char action = line[0];
                int value = int.Parse(line.Substring(1));
                ship.ProcessInstruction(action, value);
            }

            int manhattanDistance = Math.Abs(ship.x) + Math.Abs(ship.y);
            Console.WriteLine(manhattanDistance);
        }
    }
}
