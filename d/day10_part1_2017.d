
import std.stdio;
import std.file;
import std.conv;
import std.array;

void main()
{
    string input = cast(string) readText("input.txt");

    int[] lengths;
    foreach (length; input.split(","))
    {
        lengths ~= length.to!int;
    }

    int[256] list;
    foreach (i; 0..256)
    {
        list[i] = i;
    }

    int currentPosition = 0;
    int skipSize = 0;

    foreach (length; lengths)
    {
        foreach (i; 0..length/2)
        {
            int start = (currentPosition + i) % 256;
            int end = (currentPosition + length - 1 - i) % 256;
            list[start] ^= list[end];
            list[end] ^= list[start];
            list[start] ^= list[end];
        }

        currentPosition = (currentPosition + length + skipSize) % 256;
        skipSize++;
    }

    int result = list[0] * list[1];
    writeln(result);
}
