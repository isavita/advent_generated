import std.stdio;
import std.file;

void main()
{
    auto file = File("input.txt", "r");
    if (!file.isOpen)
    {
        writeln("Failed to open input.txt");
        return;
    }

    int trees = 0;
    int x = 0;
    int width = 0;

    foreach (line; file.byLine())
    {
        if (width == 0)
        {
            width = cast(int)line.length;
        }

        if (line[x % width] == '#')
        {
            trees++;
        }

        x += 3;
    }

    writeln(trees);
}