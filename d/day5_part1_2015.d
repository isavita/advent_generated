import std.stdio;
import std.file;
import std.array;
import std.string;

void main()
{
    string input = cast(string) read("input.txt");
    auto lines = input.splitLines();

    int niceCount = 0;
    foreach (line; lines)
    {
        if (isNice(line))
        {
            niceCount++;
        }
    }

    writeln(niceCount);
}

bool isNice(string s)
{
    int vowelCount = 0;
    bool hasDouble = false;
    bool hasForbidden = false;

    char prevChar = '\0';
    foreach (i, c; s)
    {
        if (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u')
        {
            vowelCount++;
        }

        if (i > 0 && c == prevChar)
        {
            hasDouble = true;
        }

        if (i > 0 && (prevChar == 'a' && c == 'b' || prevChar == 'c' && c == 'd' || prevChar == 'p' && c == 'q' || prevChar == 'x' && c == 'y'))
        {
            hasForbidden = true;
        }

        prevChar = c;
    }

    return vowelCount >= 3 && hasDouble && !hasForbidden;
}