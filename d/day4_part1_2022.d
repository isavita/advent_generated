import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.string;

void main()
{
    string data = cast(string) read("input.txt");
    auto lines = data.splitLines();
    
    int count = 0;
    
    foreach (line; lines)
    {
        auto parts = line.split(",");
        auto range1 = parts[0].split("-");
        auto range2 = parts[1].split("-");
        
        int start1 = to!int(range1[0]);
        int end1 = to!int(range1[1]);
        int start2 = to!int(range2[0]);
        int end2 = to!int(range2[1]);
        
        if ((start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1))
        {
            count++;
        }
    }
    
    writeln(count);
}