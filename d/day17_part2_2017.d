import std.stdio;
import std.file;
import std.conv;

void main()
{
    int steps;
    foreach (line; File("input.txt").byLine())
    {
        steps = to!int(line);
    }

    int position = 0;
    int valueAfterZero = 0;

    foreach (i; 1 .. 50000001)
    {
        position = (position + steps) % i;
        if (position == 0)
        {
            valueAfterZero = i;
        }
        position++;
    }

    writeln(valueAfterZero);
}