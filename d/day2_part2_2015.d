import std.file;
import std.conv;
import std.algorithm;
import std.array; // Added import for std.array
import std.stdio; // Added import for std.stdio

void main()
{
    string input = cast(string) read("input.txt");
    auto lines = input.splitter("\n");
    
    int totalPaper = 0;
    int totalRibbon = 0;
    
    foreach (line; lines)
    {
        auto dimensions = line.split("x").map!(to!int).array;
        
        int l = dimensions[0];
        int w = dimensions[1];
        int h = dimensions[2];
        
        int side1 = l * w;
        int side2 = w * h;
        int side3 = h * l;
        
        int smallestSide = min(side1, side2, side3);
        
        totalPaper += 2*l*w + 2*w*h + 2*h*l + smallestSide;
        
        int smallestPerimeter = min(2*l + 2*w, 2*w + 2*h, 2*h + 2*l);
        
        int volume = l * w * h;
        
        totalRibbon += smallestPerimeter + volume;
    }
    
    writeln(totalPaper);
    writeln(totalRibbon);
}