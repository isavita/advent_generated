import std.stdio;
import std.file;
import std.string; // Added import for splitLines

void main()
{
    string[] lines = readText("input.txt").splitLines();

    int codeChars = 0;
    int memChars = 0;

    foreach (line; lines)
    {
        codeChars += line.length;
        for (size_t i = 1; i < line.length - 1; i++)
        {
            if (line[i] == '\\')
            {
                if (line[i + 1] == '\\' || line[i + 1] == '"')
                {
                    i++;
                }
                else if (line[i + 1] == 'x')
                {
                    i += 3;
                }
            }
            memChars++;
        }
    }

    writeln(codeChars - memChars);
}