
import std.stdio;
import std.file;

void main()
{
    string input = cast(string) read("input.txt");

    int score = 0;
    int totalGarbageChars = 0;
    int groupDepth = 0;
    bool inGarbage = false;
    bool ignoreNext = false;

    foreach (char c; input)
    {
        if (ignoreNext)
        {
            ignoreNext = false;
            continue;
        }

        if (inGarbage)
        {
            if (c == '!')
            {
                ignoreNext = true;
            }
            else if (c == '>')
            {
                inGarbage = false;
            }
            else
            {
                totalGarbageChars++;
            }
        }
        else
        {
            if (c == '{')
            {
                groupDepth++;
                score += groupDepth;
            }
            else if (c == '}')
            {
                groupDepth--;
            }
            else if (c == '<')
            {
                inGarbage = true;
            }
        }
    }

    writeln("Part One: ", score);
    writeln("Part Two: ", totalGarbageChars);
}
