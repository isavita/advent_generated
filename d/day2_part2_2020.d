import std.stdio;
import std.file;
import std.conv;
import std.array; // Added import for split

void main()
{
    auto file = File("input.txt", "r");
    int validCount = 0;

    foreach (line; file.byLine())
    {
        auto parts = line.split(" ");
        auto range = parts[0].split("-");
        int pos1 = to!int(range[0]);
        int pos2 = to!int(range[1]);
        char letter = parts[1][0];
        auto password = parts[2];

        if ((password[pos1 - 1] == letter) != (password[pos2 - 1] == letter))
        {
            validCount++;
        }
    }

    writeln(validCount);
}