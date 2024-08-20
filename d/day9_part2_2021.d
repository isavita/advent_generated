import std.stdio;
import std.file;
import std.array;
import std.conv;
import std.algorithm;
import std.typecons;

void main()
{
    auto lines = File("input.txt", "r").byLine.map!(to!string).array;
    auto heightmap = lines.map!(l => l.map!(c => to!int(c.to!string)).array).array;
    
    int rows = cast(int)heightmap.length;
    int cols = cast(int)heightmap[0].length;
    
    Tuple!(int, int)[] lowPoints;
    
    foreach (i; 0 .. rows)
    {
        foreach (j; 0 .. cols)
        {
            int current = heightmap[i][j];
            if ((i == 0 || current < heightmap[i-1][j]) &&
                (i == rows-1 || current < heightmap[i+1][j]) &&
                (j == 0 || current < heightmap[i][j-1]) &&
                (j == cols-1 || current < heightmap[i][j+1]))
            {
                lowPoints ~= tuple(i, j);
            }
        }
    }
    
    auto basinSizes = lowPoints.map!(lp => findBasinSize(heightmap, lp[0], lp[1])).array.sort!"a > b";
    
    writeln(basinSizes[0] * basinSizes[1] * basinSizes[2]);
}

int findBasinSize(int[][] heightmap, int i, int j)
{
    if (i < 0 || i >= cast(int)heightmap.length || j < 0 || j >= cast(int)heightmap[0].length || heightmap[i][j] == 9)
    {
        return 0;
    }
    
    heightmap[i][j] = 9;
    
    return 1 + findBasinSize(heightmap, i-1, j) + findBasinSize(heightmap, i+1, j) + findBasinSize(heightmap, i, j-1) + findBasinSize(heightmap, i, j+1);
}