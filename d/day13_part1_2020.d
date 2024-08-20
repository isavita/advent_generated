import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.string;

void main()
{
    string input = cast(string) read("input.txt");
    auto lines = input.splitLines();
    
    int earliestTimestamp = to!int(lines[0]);
    auto busIds = lines[1].split(",").filter!(a => a != "x").map!(a => to!int(a)).array;
    
    int minWaitTime = int.max;
    int bestBusId = 0;
    
    foreach (busId; busIds)
    {
        int waitTime = busId - (earliestTimestamp % busId);
        if (waitTime < minWaitTime)
        {
            minWaitTime = waitTime;
            bestBusId = busId;
        }
    }
    
    writeln(bestBusId * minWaitTime);
}