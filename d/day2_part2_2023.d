import std.stdio;
import std.file;
import std.string;
import std.conv;

void main()
{
    string data = cast(string) read("input.txt");
    string[] games = data.strip().split("\n");

    int totalPower = 0;

    foreach (game; games)
    {
        int minRed = 0;
        int minGreen = 0;
        int minBlue = 0;

        string[] parts = game.split(":");
        string[] subsets = parts[1].split(";");

        foreach (subset; subsets)
        {
            int red = 0;
            int green = 0;
            int blue = 0;

            string[] cubes = subset.strip().split(",");

            foreach (cube; cubes)
            {
                string[] cubeParts = cube.strip().split(" ");
                int count = to!int(cubeParts[0]);

                if (cubeParts[1] == "red")
                {
                    red = count;
                }
                else if (cubeParts[1] == "green")
                {
                    green = count;
                }
                else if (cubeParts[1] == "blue")
                {
                    blue = count;
                }
            }

            if (red > minRed)
            {
                minRed = red;
            }
            if (green > minGreen)
            {
                minGreen = green;
            }
            if (blue > minBlue)
            {
                minBlue = blue;
            }
        }

        int power = minRed * minGreen * minBlue;
        totalPower += power;
    }

    writeln(totalPower);
}