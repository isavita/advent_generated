import std.stdio;
import std.file;
import std.conv;
import std.string;

void main()
{
    string input = cast(string)read("input.txt");
    input = input.strip();

    int[] lengths;
    foreach (char c; input)
    {
        lengths ~= cast(int)c;
    }
    lengths ~= [17, 31, 73, 47, 23];

    int[] list;
    foreach (i; 0 .. 256)
    {
        list ~= i;
    }

    int currentPosition = 0;
    int skipSize = 0;

    foreach (_; 0 .. 64)
    {
        foreach (length; lengths)
        {
            foreach (i; 0 .. length / 2)
            {
                int a = cast(int)((cast(ulong)currentPosition + i) % list.length);
                int b = cast(int)((cast(ulong)currentPosition + length - 1 - i) % list.length);
                list[a] ^= list[b];
                list[b] ^= list[a];
                list[a] ^= list[b];
            }
            currentPosition = cast(int)((cast(ulong)currentPosition + length + skipSize) % list.length);
            skipSize++;
        }
    }

    int[] denseHash;
    foreach (i; 0 .. 16)
    {
        int xorResult = list[i * 16];
        foreach (j; 1 .. 16)
        {
            xorResult ^= list[i * 16 + j];
        }
        denseHash ~= xorResult;
    }

    foreach (num; denseHash)
    {
        writef("%02x", num);
    }
    writeln();
}