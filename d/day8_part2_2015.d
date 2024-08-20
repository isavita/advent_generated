import std.stdio;
import std.file;
import std.conv;
import std.array;

void main()
{
    string[] lines = readText("input.txt").split("\n");

    int codeChars = 0;
    int memoryChars = 0;
    int encodedChars = 0;

    foreach (line; lines)
    {
        codeChars += line.length;
        memoryChars += countMemoryChars(line);
        encodedChars += countEncodedChars(line);
    }

    writeln(codeChars - memoryChars);
    writeln(encodedChars - codeChars);
}

int countMemoryChars(string line)
{
    int count = 0;
    for (int i = 1; i < line.length - 1; i++)
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
        count++;
    }
    return count;
}

int countEncodedChars(string line)
{
    int count = 2;
    foreach (ch; line)
    {
        if (ch == '\\' || ch == '"')
        {
            count += 2;
        }
        else
        {
            count++;
        }
    }
    return count;
}