
import std.file;
import std.conv;
import std.array;
import std.string;
import std.algorithm;
import std.stdio; // Added import for writeln

void main()
{
    string input = cast(string) read("input.txt");
    auto lines = input.splitLines();
    
    int[][] discs;
    foreach (line; lines)
    {
        auto parts = line.split(" ");
        int positions = to!int(parts[3]);
        int start = to!int(parts[parts.length - 1].stripRight("."));
        discs ~= [positions, start];
    }
    
    int time = 0;
    while (true)
    {
        bool success = true;
        foreach (i, disc; discs)
        {
            if ((disc[1] + time + i + 1) % disc[0] != 0)
            {
                success = false;
                break;
            }
        }
        
        if (success)
        {
            writeln("The first time you can press the button to get a capsule is: ", time);
            break;
        }
        
        time++;
    }
}
