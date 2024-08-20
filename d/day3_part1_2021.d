import std.stdio;
import std.file;
import std.conv;

void main()
{
    auto file = File("input.txt", "r");
    if (!file.isOpen)
    {
        writeln("Failed to open file.");
        return;
    }

    string[] lines;
    foreach (line; file.byLine())
    {
        lines ~= line.idup;
    }

    int[] zeroCounts;
    int[] oneCounts;

    foreach (line; lines)
    {
        foreach (i, c; line)
        {
            if (i >= zeroCounts.length)
            {
                zeroCounts ~= 0;
                oneCounts ~= 0;
            }

            if (c == '0')
            {
                zeroCounts[i]++;
            }
            else if (c == '1')
            {
                oneCounts[i]++;
            }
        }
    }

    string gammaRate = "";
    string epsilonRate = "";

    foreach (i, zeroCount; zeroCounts)
    {
        if (oneCounts[i] > zeroCount)
        {
            gammaRate ~= "1";
            epsilonRate ~= "0";
        }
        else
        {
            gammaRate ~= "0";
            epsilonRate ~= "1";
        }
    }

    int gammaRateDecimal = to!int(gammaRate, 2);
    int epsilonRateDecimal = to!int(epsilonRate, 2);

    int powerConsumption = gammaRateDecimal * epsilonRateDecimal;

    writeln(powerConsumption);
}