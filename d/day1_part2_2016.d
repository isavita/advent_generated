import std.stdio;
import std.file;
import std.conv;
import std.string;

void main()
{
    string input = cast(string) read("input.txt");
    string[] instructions = input.split(", ");

    int x = 0, y = 0;
    int facing = 0; // 0 = North, 1 = East, 2 = South, 3 = West
    bool firstRepeatFound = false;
    int firstRepeatDistance = 0;

    int[][] visited = [[0, 0]];

    foreach (instruction; instructions)
    {
        char turn = instruction[0];
        int distance = to!int(instruction[1 .. $]);

        if (turn == 'R')
        {
            facing = (facing + 1) % 4;
        }
        else
        {
            facing = (facing + 3) % 4;
        }

        foreach (_; 0 .. distance)
        {
            if (facing == 0)
            {
                y++;
            }
            else if (facing == 1)
            {
                x++;
            }
            else if (facing == 2)
            {
                y--;
            }
            else if (facing == 3)
            {
                x--;
            }

            foreach (coord; visited)
            {
                if (coord[0] == x && coord[1] == y)
                {
                    if (!firstRepeatFound)
                    {
                        firstRepeatDistance = abs(x) + abs(y);
                        firstRepeatFound = true;
                    }
                }
            }

            visited ~= [x, y];
        }
    }

    int totalDistance = abs(x) + abs(y);
    writeln("Part One: ", totalDistance);
    writeln("Part Two: ", firstRepeatDistance);
}

int abs(int n)
{
    return n < 0 ? -n : n;
}