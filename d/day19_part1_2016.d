import std.stdio;
import std.conv;
import std.file;

void main()
{
    auto file = File("input.txt", "r");
    auto totalElves = to!int(file.readln());

    auto highestPowerOfTwo = 1;
    while (highestPowerOfTwo * 2 <= totalElves)
    {
        highestPowerOfTwo *= 2;
    }

    writeln((totalElves - highestPowerOfTwo) * 2 + 1);
}