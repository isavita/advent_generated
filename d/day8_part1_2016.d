import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.string;

void main()
{
    auto file = File("input.txt", "r");
    auto lines = file.byLineCopy;

    bool[][] screen;
    screen.length = 6;
    foreach (ref row; screen)
    {
        row.length = 50;
    }

    foreach (line; lines)
    {
        if (line.startsWith("rect"))
        {
            auto parts = line.split(" ");
            auto dims = parts[1].split("x");
            int width = dims[0].to!int;
            int height = dims[1].to!int;

            foreach (y; 0 .. height)
            {
                foreach (x; 0 .. width)
                {
                    screen[y][x] = true;
                }
            }
        }
        else if (line.startsWith("rotate row"))
        {
            auto parts = line.split(" ");
            int y = parts[2][2..$].to!int;
            int shift = parts[4].to!int;

            bool[] newRow = new bool[](50);
            foreach (x; 0 .. 50)
            {
                newRow[(x + shift) % 50] = screen[y][x];
            }
            screen[y] = newRow;
        }
        else if (line.startsWith("rotate column"))
        {
            auto parts = line.split(" ");
            int x = parts[2][2..$].to!int;
            int shift = parts[4].to!int;

            bool[] newCol = new bool[](6);
            foreach (y; 0 .. 6)
            {
                newCol[(y + shift) % 6] = screen[y][x];
            }
            foreach (y; 0 .. 6)
            {
                screen[y][x] = newCol[y];
            }
        }
    }

    int litPixels = 0;
    foreach (y; 0 .. 6)
    {
        foreach (x; 0 .. 50)
        {
            if (screen[y][x])
            {
                litPixels++;
            }
        }
    }

    writeln(litPixels);
}