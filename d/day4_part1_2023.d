import std.stdio;
import std.file;
import std.array;
import std.conv;
import std.string;

void main()
{
    auto file = File("input.txt", "r");
    int totalPoints = 0;

    foreach (line; file.byLine())
    {
        auto parts = line.split("|");
        if (parts.length != 2)
            continue;

        auto winningNumbers = parts[0].strip().split(" ").array;
        auto yourNumbers = parts[1].strip().split(" ").array;

        int points = 0;
        foreach (yn; yourNumbers)
        {
            foreach (wn; winningNumbers)
            {
                try
                {
                    if (to!int(yn) == to!int(wn))
                    {
                        points = points == 0 ? 1 : points * 2;
                        break;
                    }
                }
                catch (ConvException)
                {
                    continue;
                }
            }
        }

        totalPoints += points;
    }

    writeln(totalPoints);
}