import std.stdio;
import std.file;
import std.conv;
import std.typecons; // Added import for Tuple

void main()
{
    int input = readInput("input.txt");
    writeln(findFirstLargerValue(input));
}

int readInput(string filename)
{
    string data = cast(string) read(filename);
    return data.to!int;
}

int findFirstLargerValue(int input)
{
    int[Tuple!(int, int)] grid;
    grid[Tuple!(int, int)(0, 0)] = 1;

    int x = 0, y = 0;
    int dx = 1, dy = 0;
    int steps = 1, stepChange = 0;

    while (true)
    {
        for (int i = 0; i < steps; i++)
        {
            x += dx;
            y += dy;

            int sum = 0;
            foreach (nx; [-1, 0, 1])
            {
                foreach (ny; [-1, 0, 1])
                {
                    if (nx == 0 && ny == 0)
                        continue;
                    sum += grid.get(Tuple!(int, int)(x + nx, y + ny), 0);
                }
            }

            if (sum > input)
            {
                return sum;
            }

            grid[Tuple!(int, int)(x, y)] = sum;
        }

        int temp = dx;
        dx = -dy;
        dy = temp;

        stepChange++;
        if (stepChange == 2)
        {
            steps++;
            stepChange = 0;
        }
    }
}