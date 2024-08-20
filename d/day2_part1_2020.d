import std.stdio;
import std.file;
import std.conv;
import std.regex;

void main()
{
    auto file = File("input.txt", "r");
    int validCount = 0;

    foreach (line; file.byLine())
    {
        auto re = regex(r"(\d+)-(\d+) (\w): (\w+)");
        auto match = line.matchFirst(re);

        if (match.length == 0)
            continue;

        int min = to!int(match[1]);
        int max = to!int(match[2]);
        char letter = match[3][0];
        string password = cast(string)match[4]; // Fixed conversion to string

        int count = 0;
        foreach (c; password)
        {
            if (c == letter)
                count++;
        }

        if (count >= min && count <= max)
            validCount++;
    }

    writeln(validCount);
}