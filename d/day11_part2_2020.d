import std.stdio;
import std.file;
import std.array;
import std.string;

void main()
{
    string[] lines = readText("input.txt").splitLines();
    auto seats = lines.dup;
    bool changed;

    do
    {
        changed = false;
        auto newSeats = seats.dup;

        foreach (i, line; seats)
        {
            foreach (j, seat; line)
            {
                if (seat == '.')
                    continue;

                int occupied = 0;
                foreach (di; -1 .. 2)
                {
                    foreach (dj; -1 .. 2)
                    {
                        if (di == 0 && dj == 0)
                            continue;

                        int ni = cast(int)(i + di);
                        int nj = cast(int)(j + dj);
                        while (ni >= 0 && ni < cast(int)(seats.length) && nj >= 0 && nj < cast(int)(seats[0].length))
                        {
                            if (seats[ni][nj] == '#')
                            {
                                occupied++;
                                break;
                            }
                            if (seats[ni][nj] == 'L')
                                break;

                            ni += cast(int)(di);
                            nj += cast(int)(dj);
                        }
                    }
                }

                if (seat == 'L' && occupied == 0)
                {
                    newSeats[i] = newSeats[i][0 .. j] ~ '#' ~ newSeats[i][j + 1 .. $];
                    changed = true;
                }
                else if (seat == '#' && occupied >= 5)
                {
                    newSeats[i] = newSeats[i][0 .. j] ~ 'L' ~ newSeats[i][j + 1 .. $];
                    changed = true;
                }
            }
        }

        seats = newSeats;
    } while (changed);

    int occupiedCount = 0;
    foreach (line; seats)
    {
        foreach (seat; line)
        {
            if (seat == '#')
                occupiedCount++;
        }
    }

    writeln(occupiedCount);
}