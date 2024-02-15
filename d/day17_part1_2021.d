import std.stdio;
import std.file;
import std.conv;
import std.string;

void main()
{
    auto file = File("input.txt", "r");
    string line = file.readln();
    auto parts = line.split(", ");
    auto xRange = parts[0][15..$].split("..");
    auto yRange = parts[1][2..$].split("..");
    int xMin = to!int(xRange[0]);
    int xMax = to!int(xRange[1]);
    int yMin = to!int(yRange[0]);
    int yMax = to!int(yRange[1]);

    int maxY = -1 << 30;
    foreach(xVel; -1000 .. 1001)
    {
        foreach(yVel; -1000 .. 1001)
        {
            int xPos = 0;
            int yPos = 0;
            int curXVel = xVel;
            int curYVel = yVel;
            int highestY = yPos;
            for (;;)
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

                if (isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax))
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

    writeln(maxY);
}

bool isMovingAway(int xPos, int yPos, int xVel, int yVel, int xMin, int xMax, int yMin, int yMax)
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