import std.stdio;
import std.file;
import std.array;
import std.conv;

void main()
{
    auto file = File("input.txt", "r");
    auto lines = file.byLineCopy();
    auto grid = lines.array();

    foreach(i; 0 .. 100)
    {
        grid = animate(grid);
    }

    int lightsOn = countLightsOn(grid);
    writeln(lightsOn);
}

string[] animate(string[] grid)
{
    string[] newGrid = grid.dup;

    foreach(y, line; grid)
    {
        foreach(x, light; line)
        {
            int neighborsOn = countNeighborsOn(grid, cast(int)x, cast(int)y);
            if(light == '#' && (neighborsOn < 2 || neighborsOn > 3))
            {
                newGrid[y] = newGrid[y][0 .. x] ~ '.' ~ newGrid[y][x + 1 .. $];
            }
            else if(light == '.' && neighborsOn == 3)
            {
                newGrid[y] = newGrid[y][0 .. x] ~ '#' ~ newGrid[y][x + 1 .. $];
            }
        }
    }

    return newGrid;
}

int countNeighborsOn(string[] grid, int x, int y)
{
    int count = 0;
    foreach(dy; -1 .. 2)
    {
        foreach(dx; -1 .. 2)
        {
            if(dx == 0 && dy == 0) continue;
            int nx = x + dx;
            int ny = y + dy;
            if(nx >= 0 && nx < 100 && ny >= 0 && ny < 100 && grid[ny][nx] == '#')
            {
                count++;
            }
        }
    }
    return count;
}

int countLightsOn(string[] grid)
{
    int count = 0;
    foreach(line; grid)
    {
        foreach(light; line)
        {
            if(light == '#')
            {
                count++;
            }
        }
    }
    return count;
}