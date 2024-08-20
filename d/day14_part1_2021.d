import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.string; // Added import for std.string

void main()
{
    string input = cast(string) read("input.txt");
    string[] lines = input.split("\n");

    string polymerTemplate = lines[0];
    auto pairInsertionRules = lines[2..$].array;

    foreach (i, rule; pairInsertionRules)
    {
        pairInsertionRules[i] = rule.replace(" -> ", "");
    }

    foreach (_; 0 .. 10)
    {
        string newPolymer = polymerTemplate[0..1];
        foreach (i; 0 .. polymerTemplate.length - 1)
        {
            string pair = polymerTemplate[i..i+2];
            foreach (rule; pairInsertionRules)
            {
                if (pair == rule[0..2])
                {
                    newPolymer ~= rule[2];
                    break;
                }
            }
            newPolymer ~= pair[1];
        }
        polymerTemplate = newPolymer;
    }

    int[string] elementCounts; // Changed to use associative array
    foreach (char c; polymerTemplate)
    {
        elementCounts[c.to!string]++; // Convert char to string
    }

    int minCount = int.max;
    int maxCount = int.min;
    foreach (count; elementCounts)
    {
        if (count < minCount)
        {
            minCount = count;
        }
        if (count > maxCount)
        {
            maxCount = count;
        }
    }

    writeln(maxCount - minCount);
}