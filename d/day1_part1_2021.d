import std.stdio;
import std.file;
import std.conv;

void main()
{
    int prev = 0;
    int current = 0;
    int count = 0;

    auto file = File("input.txt", "r");
    foreach(line; file.byLine())
    {
        current = to!int(line);
        if(prev != 0 && current > prev)
        {
            count++;
        }
        prev = current;
    }

    writeln(count);
}