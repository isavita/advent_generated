
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string[] lines = File.ReadAllLines("input.txt");
        string[] parts = lines[0].Split(", ");
        string[] xRange = parts[0].Substring(15).Split("..");
        string[] yRange = parts[1].Substring(2).Split("..");
        int xMin = int.Parse(xRange[0]);
        int xMax = int.Parse(xRange[1]);
        int yMin = int.Parse(yRange[0]);
        int yMax = int.Parse(yRange[1]);

        int maxY = int.MinValue;
        for (int xVel = -1000; xVel <= 1000; xVel++)
        {
            for (int yVel = -1000; yVel <= 1000; yVel++)
            {
                int xPos = 0, yPos = 0;
                int curXVel = xVel, curYVel = yVel;
                int highestY = yPos;
                while (true)
                {
                    xPos += curXVel;
                    yPos += curYVel;

                    if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax)
                    {
                        if (highestY > maxY)
                        {
                            maxY = highestY;
                        }
                        break;
                    }

                    if (IsMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax))
                    {
                        break;
                    }

                    if (curXVel > 0)
                    {
                        curXVel--;
                    }
                    else if (curXVel < 0)
                    {
                        curXVel++;
                    }

                    curYVel--;
                    if (yPos > highestY)
                    {
                        highestY = yPos;
                    }
                }
            }
        }

        Console.WriteLine(maxY);
    }

    static bool IsMovingAway(int xPos, int yPos, int xVel, int yVel, int xMin, int xMax, int yMin, int yMax)
    {
        if (xPos < xMin && xVel < 0)
        {
            return true;
        }
        if (xPos > xMax && xVel > 0)
        {
            return true;
        }
        if (yPos < yMin && yVel < 0)
        {
            return true;
        }
        return false;
    }
}
