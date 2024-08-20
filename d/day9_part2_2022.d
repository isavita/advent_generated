import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm; // Added import for map
import std.string; // Added import for split

void main()
{
    auto file = File("input.txt", "r");
    auto lines = file.byLine.map!(a => a.idup).array;
    file.close();

    writeln("Part One: ", partOne(lines));
    writeln("Part Two: ", partTwo(lines));
}

ulong partOne(string[] lines) // Changed return type to ulong
{
    int[string] visited; // Changed to int[string]
    int headX = 0, headY = 0;
    int tailX = 0, tailY = 0;

    visited[to!string(tailX) ~ "," ~ to!string(tailY)] = 1;

    foreach (line; lines)
    {
        auto parts = line.split(" ");
        string direction = parts[0];
        int steps = to!int(parts[1]);

        foreach (_; 0 .. steps)
        {
            switch (direction)
            {
                case "R": headX++; break;
                case "L": headX--; break;
                case "U": headY++; break;
                case "D": headY--; break;
                default: assert(0);
            }

            if (abs(headX - tailX) > 1 || abs(headY - tailY) > 1)
            {
                if (headX != tailX)
                    tailX += (headX - tailX) / abs(headX - tailX);
                if (headY != headY)
                    tailY += (headY - tailY) / abs(headY - tailY);
            }

            visited[to!string(tailX) ~ "," ~ to!string(tailY)] = 1;
        }
    }

    return cast(ulong)visited.length; // Cast to ulong
}

ulong partTwo(string[] lines) // Changed return type to ulong
{
    int[string] visited; // Changed to int[string]
    int[10] x = 0;
    int[10] y = 0;

    visited[to!string(x[9]) ~ "," ~ to!string(y[9])] = 1;

    foreach (line; lines)
    {
        auto parts = line.split(" ");
        string direction = parts[0];
        int steps = to!int(parts[1]);

        foreach (_; 0 .. steps)
        {
            switch (direction)
            {
                case "R": x[0]++; break;
                case "L": x[0]--; break;
                case "U": y[0]++; break;
                case "D": y[0]--; break;
                default: assert(0);
            }

            foreach (i; 1 .. 10)
            {
                if (abs(x[i-1] - x[i]) > 1 || abs(y[i-1] - y[i]) > 1)
                {
                    if (x[i-1] != x[i])
                        x[i] += (x[i-1] - x[i]) / abs(x[i-1] - x[i]);
                    if (y[i-1] != y[i])
                        y[i] += (y[i-1] - y[i]) / abs(y[i-1] - y[i]);
                }
            }

            visited[to!string(x[9]) ~ "," ~ to!string(y[9])] = 1;
        }
    }

    return cast(ulong)visited.length; // Cast to ulong
}

int abs(int value)
{
    return value < 0 ? -value : value;
}