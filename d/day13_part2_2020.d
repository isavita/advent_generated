import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.string;

void main()
{
    string input = cast(string)std.file.read("input.txt");
    string[] lines = input.splitLines();

    string[] busIDs = lines[1].split(",");

    long timestamp = 0;
    long step = to!long(busIDs[0]);

    foreach (i, id; busIDs)
    {
        if (id == "x")
            continue;

        long busID = to!long(id);

        while ((timestamp + i) % busID != 0)
        {
            timestamp += step;
        }

        step *= busID;
    }

    writeln(timestamp);
}