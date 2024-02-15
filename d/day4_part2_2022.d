
import std.stdio;
import std.array;
import std.conv;

void main()
{
    auto file = File("input.txt", "r");

    int count = 0;
    foreach (line; file.byLine())
    {
        auto pair = line.split(",");
        
        auto left = parseRange(pair[0].idup); // idup to create a copy of the string
        auto right = parseRange(pair[1].idup);

        if (left[0] <= right[1] && left[1] >= right[0])
        {
            count++;
        }
    }

    writeln(count);
}

int[] parseRange(string s)
{
    auto split = s.split("-");
    int start = to!int(split[0]);
    int end = to!int(split[1]);
    return [start, end];
}
