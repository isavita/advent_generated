import std.stdio;
import std.file;
import std.array;
import std.conv;
import std.string;

void main()
{
    string input = cast(string) read("input.txt");
    auto lines = input.split("\n");
    int rows = cast(int) lines.length;
    int cols = cast(int) lines[0].length;

    int visibleTrees = 2 * (rows + cols - 2);

    foreach (i; 1 .. rows - 1)
    {
        foreach (j; 1 .. cols - 1)
        {
            int height = to!int(lines[i][j]);

            bool visible = true;
            foreach_reverse (k; 0 .. j)
            {
                if (to!int(lines[i][k]) >= height)
                {
                    visible = false;
                    break;
                }
            }
            if (visible)
            {
                visibleTrees++;
                continue;
            }

            visible = true;
            foreach (k; j + 1 .. cols)
            {
                if (to!int(lines[i][k]) >= height)
                {
                    visible = false;
                    break;
                }
            }
            if (visible)
            {
                visibleTrees++;
                continue;
            }

            visible = true;
            foreach_reverse (k; 0 .. i)
            {
                if (to!int(lines[k][j]) >= height)
                {
                    visible = false;
                    break;
                }
            }
            if (visible)
            {
                visibleTrees++;
                continue;
            }

            visible = true;
            foreach (k; i + 1 .. rows)
            {
                if (to!int(lines[k][j]) >= height)
                {
                    visible = false;
                    break;
                }
            }
            if (visible)
            {
                visibleTrees++;
                continue;
            }
        }
    }

    writeln(visibleTrees);
}