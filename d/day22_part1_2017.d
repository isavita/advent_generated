import std.stdio;
import std.array;
import std.algorithm;
import std.file;

void main()
{
    string[] lines = File("input.txt").byLineCopy.array;
    int gridSize = cast(int)lines.length;
    int virusX = gridSize / 2;
    int virusY = gridSize / 2;
    int virusDir = 0; // 0: up, 1: right, 2: down, 3: left
    int infections = 0;

    bool[][] infected = new bool[][](gridSize, gridSize);
    foreach (i, line; lines)
    {
        foreach (j, c; line)
        {
            infected[i][j] = c == '#';
        }
    }

    foreach (_; 0 .. 10000)
    {
        if (infected[virusY][virusX])
        {
            virusDir = (virusDir + 1) % 4;
            infected[virusY][virusX] = false;
        }
        else
        {
            virusDir = (virusDir + 3) % 4;
            infected[virusY][virusX] = true;
            infections++;
        }

        switch (virusDir)
        {
            case 0: virusY--; break;
            case 1: virusX++; break;
            case 2: virusY++; break;
            case 3: virusX--; break;
            default: assert(0);
        }

        if (virusY < 0 || virusY >= gridSize || virusX < 0 || virusX >= gridSize)
        {
            int newSize = gridSize * 2;
            bool[][] newInfected = new bool[][](newSize, newSize);
            foreach (i, row; infected)
            {
                foreach (j, cell; row)
                {
                    newInfected[i + gridSize / 2][j + gridSize / 2] = cell;
                }
            }
            infected = newInfected;
            virusX += gridSize / 2;
            virusY += gridSize / 2;
            gridSize = newSize;
        }
    }

    writeln(infections);
}