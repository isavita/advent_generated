
import std.stdio;
import std.algorithm;
import std.array;
import std.conv;
import std.string;
import std.file;

void main()
{
    string input = cast(string) read("input.txt");
    string[] lines = input.splitLines();
    
    string[] programsAbove = [];
    string[] programsBelow = [];
    
    foreach (line; lines)
    {
        auto parts = line.split(" -> ");
        programsBelow ~= parts[0].split(" ")[0];
        if (parts.length > 1)
        {
            programsAbove ~= parts[1].split(", ").array();
        }
    }
    
    writeln(programsBelow.find!(a => !programsAbove.canFind(a)));
}
