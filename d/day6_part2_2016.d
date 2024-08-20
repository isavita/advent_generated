import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.string;

void main()
{
    auto file = File("input.txt", "r");
    auto lines = file.byLine.map!(a => a.idup).array;
    file.close();

    size_t length = lines[0].length;
    string message = "";
    string originalMessage = "";

    foreach (i; 0 .. length)
    {
        int[char] count;
        foreach (line; lines)
        {
            count[line[i]]++;
        }

        char mostCommon;
        int mostCommonCount = -1;
        char leastCommon;
        int leastCommonCount = int.max;

        foreach (k, v; count)
        {
            if (v > mostCommonCount)
            {
                mostCommon = k;
                mostCommonCount = v;
            }
            if (v < leastCommonCount)
            {
                leastCommon = k;
                leastCommonCount = v;
            }
        }

        message ~= mostCommon;
        originalMessage ~= leastCommon;
    }

    writeln(message);
    writeln(originalMessage);
}