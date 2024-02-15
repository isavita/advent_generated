
import std.stdio;
import std.algorithm;
import std.conv;
import std.range;
import std.array;

void main()
{
    auto input = File("input.txt", "r").byLine.map!(to!int).array.sort;
    int oneJoltDiff = 0;
    int threeJoltDiff = 1; // Start at 1 for the built-in adapter
    int prevJoltage = 0;
    
    foreach (joltage; input)
    {
        if (joltage - prevJoltage == 1)
        {
            oneJoltDiff++;
        }
        else if (joltage - prevJoltage == 3)
        {
            threeJoltDiff++;
        }
        
        prevJoltage = joltage;
    }
    
    writeln(oneJoltDiff * threeJoltDiff);
}
