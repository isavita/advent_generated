import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;

void main()
{
    auto file = File("input.txt", "r");

    int twoCount = 0;
    int threeCount = 0;

    foreach(line; file.byLine())
    {
        auto id = line.idup;

        auto twosAndThrees = id.map!(c => id.count(c)).array;
        
        if (twosAndThrees.count(2))
        {
            twoCount++;
        }
        
        if (twosAndThrees.count(3))
        {
            threeCount++;
        }
    }

    file.close();

    writefln("%s", twoCount * threeCount);
}