import std.stdio;
import std.file;
import std.array;
import std.algorithm;
import std.string;

void main()
{
    auto file = File("input.txt", "r");
    auto lines = file.byLine.map!(a => a.idup).array;
    file.close();

    int count = 0;
    foreach (line; lines)
    {
        auto parts = line.split("|");
        auto outputValues = parts[1].strip.split(" ");

        foreach (value; outputValues)
        {
            if (value.length == 2 || value.length == 4 || value.length == 3 || value.length == 7)
            {
                count++;
            }
        }
    }

    writeln(count);
}