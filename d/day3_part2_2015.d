
import std.stdio;
import std.file;
import std.conv;

void main()
{
    string directions = cast(string)read("input.txt");
    int[int[2]] visitedHouses;
    int xSanta = 0, ySanta = 0;
    int xRobo = 0, yRobo = 0;
    bool isSantaTurn = true;

    visitedHouses[[xSanta, ySanta]] = 1;

    foreach (char dir; directions)
    {
        int* x;
        int* y;

        if (isSantaTurn)
        {
            x = &xSanta;
            y = &ySanta;
        }
        else
        {
            x = &xRobo;
            y = &yRobo;
        }

        switch (dir)
        {
            case '^':
                ++*y;
                break;
            case 'v':
                --*y;
                break;
            case '>':
                ++*x;
                break;
            case '<':
                --*x;
                break;
            default:
                break;
        }

        visitedHouses[[*x, *y]] = 1;
        isSantaTurn = !isSantaTurn;
    }

    writeln(visitedHouses.length);
}
