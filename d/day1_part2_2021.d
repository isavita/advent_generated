
import std.stdio;
import std.conv;
import std.file;

void main()
{
    int count = 0;
    int[] vals;

    foreach(line; File("input.txt").byLine)
    {
        if (line.length == 0)
            continue;
        
        int val = to!int(line);
        vals ~= val;
    }

    int prevSum = vals[0] + vals[1] + vals[2];

    for (size_t i = 3; i < vals.length; i++)
    {
        int currSum = vals[i-2] + vals[i-1] + vals[i];
        if (currSum > prevSum)
            count++;
        
        prevSum = currSum;
    }

    writeln(count);
}
