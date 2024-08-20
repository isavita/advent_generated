import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.string;

void main()
{
    string data = cast(string) readText("input.txt");
    auto fish = data.split(",").map!(to!int).array;

    foreach (_; 0 .. 80)
    {
        int newFish = 0;
        foreach (i, f; fish)
        {
            if (f == 0)
            {
                fish[i] = 6;
                newFish++;
            }
            else
            {
                fish[i]--;
            }
        }
        fish ~= repeat(8, newFish);
    }

    writeln(fish.length);
}

int[] repeat(int value, int times)
{
    int[] result;
    foreach (_; 0 .. times)
    {
        result ~= value;
    }
    return result;
}