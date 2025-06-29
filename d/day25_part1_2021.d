
import std.stdio;
import std.file;
import std.string;
import std.array;
import std.algorithm;

/**
 * Represents a coordinate on the grid.
 * Using size_t as it's the canonical type for array indexing.
 */
struct Point
{
    size_t r; // row
    size_t c; // column
}

/**
 * Simulates one phase of movement for a specific herd.
 *
 * To handle simultaneous movement, it first identifies all cucumbers that can move
 * and then updates their positions on the grid.
 *
 * Params:
 *   grid = The sea cucumber grid, passed by reference to be modified.
 *   herdChar = The character representing the herd to move ('>' or 'v').
 *   dr = The row change for this herd's movement (0 for east, 1 for south).
 *   dc = The column change for this herd's movement (1 for east, 0 for south).
 *
 * Returns: true if any cucumbers of this herd moved, false otherwise.
 */
bool moveHerd(ref char[][] grid, char herdChar, int dr, int dc)
{
    const size_t height = grid.length;
    const size_t width = grid[0].length;

    // 1. Find all cucumbers of the specified herd that can move.
    Point[] movers;
    foreach (r; 0 .. height)
    {
        foreach (c; 0 .. width)
        {
            if (grid[r][c] == herdChar)
            {
                // Calculate the destination, handling grid wrap-around with modulo.
                auto nextR = (r + dr) % height;
                auto nextC = (c + dc) % width;

                if (grid[nextR][nextC] == '.')
                {
                    movers ~= Point(r, c);
                }
            }
        }
    }

    // If no cucumbers can move, we're done with this phase.
    if (movers.length == 0)
    {
        return false;
    }

    // 2. Apply all moves simultaneously by updating the grid.
    foreach (p; movers)
    {
        auto nextR = (p.r + dr) % height;
        auto nextC = (p.c + dc) % width;
        grid[p.r][p.c] = '.';
        grid[nextR][nextC] = herdChar;
    }

    return true;
}

/**
 * Main entry point. Reads the grid, runs the simulation, and prints the result.
 */
void main()
{
    // Read and parse the input grid from "input.txt".
    char[][] grid;
    try
    {
        // .strip() removes trailing newlines, .splitLines() gets each row.
        // .map!(a => a.dup).array creates a mutable char[][] from immutable slices.
        grid = readText("input.txt").strip.splitLines.map!(a => a.dup).array;
    }
    catch (FileException e)
    {
        stderr.writeln("Error: Could not read input.txt. ", e.msg);
        return;
    }

    // Handle empty or malformed input. If no cucumbers exist, they stop on step 1.
    if (grid.length == 0 || grid[0].length == 0)
    {
        writeln(1);
        return;
    }

    // Run the simulation loop, counting steps.
    for (int step = 1; ; ++step)
    {
        // A full step consists of an east phase followed by a south phase.
        bool movedEast = moveHerd(grid, '>', 0, 1);
        bool movedSouth = moveHerd(grid, 'v', 1, 0);

        // If no cucumbers moved in either phase, the system is stable.
        if (!movedEast && !movedSouth)
        {
            writeln(step);
            break;
        }
    }
}
