import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;

void main()
{
    string input = cast(string) read("input.txt");
    auto banks = input.split().map!(to!int).array;

    auto seen = [banks.idup: 0];
    int cycles = 0;

    for (;;)
    {
        cycles++;
        int maxBlocks = cast(int) banks.maxElement;
        int index = cast(int) banks.countUntil(maxBlocks);

        banks[index] = 0;
        for (int i = 0; i < maxBlocks; i++)
        {
            index = cast(int)((cast(ulong)(index + 1)) % banks.length);
            banks[index]++;
        }

        if (banks.idup in seen)
        {
            writeln(cycles - seen[banks.idup]);
            break;
        }
        seen[banks.idup] = cycles;
    }
}