import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.string;

void main()
{
    string data = cast(string)std.file.read("input.txt");
    auto lines = data.splitLines();

    int[string] blackTiles;

    foreach (line; lines)
    {
        int x = 0;
        int y = 0;

        for (int i = 0; i < line.length; i++)
        {
            char dir = line[i];
            if (dir == 'e')
            {
                x += 1;
            }
            else if (dir == 'w')
            {
                x -= 1;
            }
            else if (dir == 'n')
            {
                i++;
                if (line[i] == 'e')
                {
                    y -= 1;
                }
                else if (line[i] == 'w')
                {
                    x -= 1;
                    y -= 1;
                }
            }
            else if (dir == 's')
            {
                i++;
                if (line[i] == 'e')
                {
                    x += 1;
                    y += 1;
                }
                else if (line[i] == 'w')
                {
                    y += 1;
                }
            }
        }

        string key = x.to!string ~ "," ~ y.to!string;
        if (key in blackTiles)
        {
            blackTiles.remove(key);
        }
        else
        {
            blackTiles[key] = 1;
        }
    }

    writeln(blackTiles.length);
}