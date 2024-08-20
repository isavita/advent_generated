import std.stdio;
import std.file;
import std.array;
import std.algorithm;
import std.conv;

void main()
{
    string data = cast(string) read("input.txt");
    auto lines = data.splitter("\n").array;

    int sumPart1 = 0;
    int sumPart2 = 0;

    foreach (i, line; lines)
    {
        auto half = line.length / 2;
        auto firstHalf = line[0..half];
        auto secondHalf = line[half..$];

        foreach (char c; firstHalf)
        {
            if (secondHalf.canFind(c))
            {
                sumPart1 += getPriority(c);
                break;
            }
        }

        if (i % 3 == 0 && i + 2 < lines.length)
        {
            foreach (char c; line)
            {
                if (lines[i + 1].canFind(c) && lines[i + 2].canFind(c))
                {
                    sumPart2 += getPriority(c);
                    break;
                }
            }
        }
    }

    writeln(sumPart1);
    writeln(sumPart2);
}

int getPriority(char c)
{
    if (c >= 'a' && c <= 'z')
    {
        return cast(int)(c - 'a' + 1);
    }
    else if (c >= 'A' && c <= 'Z')
    {
        return cast(int)(c - 'A' + 27);
    }
    return 0;
}