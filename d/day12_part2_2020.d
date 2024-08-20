import std.stdio;
import std.file;
import std.conv;

void main()
{
    auto file = File("input.txt", "r");
    if (!file.isOpen)
    {
        writeln("Failed to open file.");
        return;
    }

    int shipEast = 0;
    int shipNorth = 0;
    int waypointEast = 10;
    int waypointNorth = 1;

    foreach (line; file.byLine())
    {
        if (line.length == 0)
            continue;

        char action = line[0];
        int value = to!int(line[1..$]);

        switch (action)
        {
            case 'N':
                waypointNorth += value;
                break;
            case 'S':
                waypointNorth -= value;
                break;
            case 'E':
                waypointEast += value;
                break;
            case 'W':
                waypointEast -= value;
                break;
            case 'L':
                foreach_reverse (_; 0 .. value / 90)
                {
                    int temp = waypointEast;
                    waypointEast = -waypointNorth;
                    waypointNorth = temp;
                }
                break;
            case 'R':
                foreach (_; 0 .. value / 90)
                {
                    int temp = waypointEast;
                    waypointEast = waypointNorth;
                    waypointNorth = -temp;
                }
                break;
            case 'F':
                shipEast += waypointEast * value;
                shipNorth += waypointNorth * value;
                break;
            default:
                writeln("Unknown action: ", action);
                break;
        }
    }

    int manhattanDistance = abs(shipEast) + abs(shipNorth);
    writeln(manhattanDistance);
}

int abs(int x)
{
    return x < 0 ? -x : x;
}