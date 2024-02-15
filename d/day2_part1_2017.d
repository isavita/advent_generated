
import std.stdio;
import std.file;
import std.string;
import std.conv;

void main()
{
    string data = cast(string) read("input.txt");
    string[] lines = data.splitLines;
    int checksum = 0;

    foreach (line; lines)
    {
        string[] nums = line.split;
        int minVal = to!int(nums[0]);
        int maxVal = to!int(nums[0]);

        foreach (numStr; nums)
        {
            int num = to!int(numStr);
            if (num < minVal)
            {
                minVal = num;
            }
            if (num > maxVal)
            {
                maxVal = num;
            }
        }

        checksum += (maxVal - minVal);
    }

    writeln(checksum);
}
