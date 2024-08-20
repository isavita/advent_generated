import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.string;
import std.algorithm; // Added import for map

void main()
{
    auto file = File("input.txt", "r");
    auto lines = file.byLine.map!(a => a.idup).array;
    file.close();

    int delay = 0;
    while (true)
    {
        bool caught = false;
        foreach (line; lines)
        {
            auto parts = line.split(": ");
            int depth = to!int(parts[0]);
            int range = to!int(parts[1]);

            if ((depth + delay) % ((range - 1) * 2) == 0)
            {
                caught = true;
                break;
            }
        }

        if (!caught)
        {
            writeln(delay);
            return;
        }

        delay++;
    }
}