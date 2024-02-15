
import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.string;

void main()
{
    string input = cast(string) read("input.txt");
    auto startingNumbers = input.strip().split(",");

    int[int] lastSpoken;
    int lastNumber, nextNumber;

    foreach (turn; 1 .. 2021)
    {
        if (turn - 1 < startingNumbers.length)
        {
            lastNumber = startingNumbers[turn - 1].to!int;
            lastSpoken[lastNumber] = turn;
            continue;
        }

        if (lastNumber in lastSpoken && lastSpoken[lastNumber] != turn - 1)
        {
            nextNumber = turn - 1 - lastSpoken[lastNumber];
        }
        else
        {
            nextNumber = 0;
        }

        lastSpoken[lastNumber] = turn - 1;
        lastNumber = nextNumber;
    }

    writeln(lastNumber);
}
