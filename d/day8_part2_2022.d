import std.stdio;
import std.file;
import std.array;
import std.conv;

void main()
{
    string input = cast(string)std.file.read("input.txt");
    auto lines = input.split("\n");
    
    int rows = cast(int)lines.length;
    int cols = cast(int)lines[0].length;
    
    int[][] grid;
    foreach (line; lines)
    {
        int[] row;
        foreach (ch; line)
        {
            row ~= ch.to!int;
        }
        grid ~= row;
    }
    
    int maxScenicScore = 0;
    
    foreach (i; 1 .. rows - 1)
    {
        foreach (j; 1 .. cols - 1)
        {
            int height = grid[i][j];
            
            int up = 0;
            foreach_reverse (k; 0 .. i)
            {
                up++;
                if (grid[k][j] >= height)
                    break;
            }
            
            int down = 0;
            foreach (k; i + 1 .. rows)
            {
                down++;
                if (grid[k][j] >= height)
                    break;
            }
            
            int left = 0;
            foreach_reverse (k; 0 .. j)
            {
                left++;
                if (grid[i][k] >= height)
                    break;
            }
            
            int right = 0;
            foreach (k; j + 1 .. cols)
            {
                right++;
                if (grid[i][k] >= height)
                    break;
            }
            
            int scenicScore = up * down * left * right;
            if (scenicScore > maxScenicScore)
                maxScenicScore = scenicScore;
        }
    }
    
    writeln(maxScenicScore);
}