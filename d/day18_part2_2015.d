import std.stdio;
import std.file;

void main()
{
    auto file = File("input.txt", "r");
    string[] grid;
    foreach (line; file.byLine())
    {
        grid ~= line.idup;
    }
    file.close();

    foreach (i; 0 .. 100)
    {
        grid[0] = "#" ~ grid[0][1 .. $-1] ~ "#";
        grid[$-1] = "#" ~ grid[$-1][1 .. $-1] ~ "#";
        grid = step(grid);
    }

    int count = 0;
    foreach (line; grid)
    {
        foreach (c; line)
        {
            if (c == '#')
            {
                count++;
            }
        }
    }

    writeln(count);
}

string[] step(string[] grid)
{
    string[] newGrid = grid.dup;
    foreach (y, line; grid)
    {
        foreach (x, c; line)
        {
            if ((x == 0 && y == 0) || (x == 0 && y == grid.length-1) || (x == line.length-1 && y == 0) || (x == line.length-1 && y == grid.length-1))
            {
                newGrid[y] = newGrid[y][0 .. x] ~ "#" ~ newGrid[y][x+1 .. $];
                continue;
            }

            int onNeighbors = countOnNeighbors(grid, cast(int)x, cast(int)y);
            if (c == '#' && (onNeighbors < 2 || onNeighbors > 3))
            {
                newGrid[y] = newGrid[y][0 .. x] ~ "." ~ newGrid[y][x+1 .. $];
            }
            else if (c == '.' && onNeighbors == 3)
            {
                newGrid[y] = newGrid[y][0 .. x] ~ "#" ~ newGrid[y][x+1 .. $];
            }
        }
    }
    return newGrid;
}

int countOnNeighbors(string[] grid, int x, int y)
{
    int count = 0;
    foreach (dx; -1 .. 2)
    {
        foreach (dy; -1 .. 2)
        {
            if (dx == 0 && dy == 0)
            {
                continue;
            }
            int nx = x + dx;
            int ny = y + dy;
            if (nx >= 0 && nx < grid[0].length && ny >= 0 && ny < grid.length && grid[ny][nx] == '#')
            {
                count++;
            }
        }
    }
    return count;
}