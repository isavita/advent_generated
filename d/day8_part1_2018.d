import std.stdio;
import std.file;
import std.conv;
import std.array;

void main()
{
    string input = cast(string)std.file.read("input.txt");
    string[] parts = input.split(" ");
    int[] numbers;

    foreach(part; parts)
    {
        numbers ~= to!int(part);
    }

    auto result = parseTree(numbers, 0);
    writeln(result[0]);
}

int[] parseTree(int[] data, int index)
{
    int childCount = data[index];
    int metaCount = data[index + 1];
    index += 2;

    int sum = 0;
    foreach(_; 0 .. childCount)
    {
        auto childSumIndex = parseTree(data, index);
        sum += childSumIndex[0];
        index = childSumIndex[1];
    }

    foreach(_; 0 .. metaCount)
    {
        sum += data[index];
        index++;
    }

    return [sum, index];
}