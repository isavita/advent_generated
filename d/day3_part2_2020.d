import std.stdio;
import std.array;
import std.algorithm;
import std.file;

void main()
{
    string[] lines = File("input.txt", "r").byLineCopy.array;
    
    int width = cast(int)lines[0].length;
    int height = cast(int)lines.length;
    
    int[][] slopes = [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]];
    
    int result = 1;
    
    foreach (slope; slopes)
    {
        int trees = 0;
        int x = 0;
        int y = 0;
        
        while (y < height)
        {
            if (lines[y][x % width] == '#')
            {
                trees++;
            }
            
            x += slope[0];
            y += slope[1];
        }
        
        result *= trees;
    }
    
    writeln(result);
}