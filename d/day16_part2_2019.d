import std.stdio;
import std.file;
import std.conv;

void main()
{
    string input = cast(string) read("input.txt");
    int[] signal;
    foreach (char c; input)
    {
        signal ~= to!int(c.to!string);
    }

    int offset = 0;
    foreach (i; 0 .. 7)
    {
        offset = offset * 10 + signal[i];
    }

    int[] realSignal;
    foreach (_; 0 .. 10000)
    {
        realSignal ~= signal;
    }

    foreach (_; 0 .. 100)
    {
        int sum = 0;
        for (int i = cast(int)realSignal.length - 1; i >= offset; i--)
        {
            sum = (sum + realSignal[i]) % 10;
            realSignal[i] = sum;
        }
    }

    foreach (i; offset .. offset + 8)
    {
        write(realSignal[i]);
    }
    writeln();
}