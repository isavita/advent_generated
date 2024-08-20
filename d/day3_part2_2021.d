import std.stdio;
import std.file;
import std.conv;

void main()
{
    auto file = File("input.txt", "r");
    string[] lines;
    foreach (line; file.byLine())
    {
        lines ~= line.idup;
    }
    file.close();

    int gammaRate = 0;
    int epsilonRate = 0;

    foreach (i; 0 .. cast(int)lines[0].length)
    {
        int count0 = 0;
        int count1 = 0;
        foreach (line; lines)
        {
            if (line[i] == '0')
            {
                count0++;
            }
            else
            {
                count1++;
            }
        }
        if (count1 > count0)
        {
            gammaRate = (gammaRate << 1) | 1;
            epsilonRate = epsilonRate << 1;
        }
        else
        {
            gammaRate = gammaRate << 1;
            epsilonRate = (epsilonRate << 1) | 1;
        }
    }

    writeln("Power consumption: ", gammaRate * epsilonRate);

    int oxygenGeneratorRating = findRating(lines, true);
    int co2ScrubberRating = findRating(lines, false);
    writeln("Life support rating: ", oxygenGeneratorRating * co2ScrubberRating);
}

int findRating(string[] lines, bool mostCommon)
{
    foreach (i; 0 .. cast(int)lines[0].length)
    {
        int count0 = 0;
        int count1 = 0;
        foreach (line; lines)
        {
            if (line[i] == '0')
            {
                count0++;
            }
            else
            {
                count1++;
            }
        }
        char bitCriteria;
        if (mostCommon)
        {
            bitCriteria = count1 >= count0 ? '1' : '0';
        }
        else
        {
            bitCriteria = count0 <= count1 ? '0' : '1';
        }
        lines = filterLines(lines, cast(int)i, bitCriteria);
        if (lines.length == 1)
        {
            break;
        }
    }
    return to!int(lines[0], 2);
}

string[] filterLines(string[] lines, int index, char bitCriteria)
{
    string[] filteredLines;
    foreach (line; lines)
    {
        if (line[index] == bitCriteria)
        {
            filteredLines ~= line;
        }
    }
    return filteredLines;
}