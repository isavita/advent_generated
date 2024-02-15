
import std.stdio;
import std.file;
import std.array;
import std.conv;

void main()
{
	auto file = File("input.txt", "r");
	auto lines = file.byLineCopy();

	string[] grid;
	foreach (line; lines)
	{
		grid ~= line.idup;
	}

	int x = 0;
	int y = 0;
	foreach (size_t i, c; grid[0]) // Changed 'int' to 'size_t' to match the type of 'i'
	{
		if (c == '|')
		{
			x = cast(int)i; // Cast 'i' to int to assign it to 'x'
			break;
		}
	}

	int dx = 0;
	int dy = 1;
	int steps = 0;

	while (x >= 0 && x < grid[0].length && y >= 0 && y < grid.length)
	{
		char cell = grid[y][x];

		if (cell == ' ')
		{
			break;
		}

		++steps;

		if (cell == '+')
		{
			if (dx == 0)
			{
				if (x > 0 && (grid[y][x - 1] == '-' || (grid[y][x - 1] >= 'A' && grid[y][x - 1] <= 'Z')))
				{
					dx = -1;
					dy = 0;
				}
				else
				{
					dx = 1;
					dy = 0;
				}
			}
			else
			{
				if (y > 0 && (grid[y - 1][x] == '|' || (grid[y - 1][x] >= 'A' && grid[y - 1][x] <= 'Z')))
				{
					dx = 0;
					dy = -1;
				}
				else
				{
					dx = 0;
					dy = 1;
				}
			}
		}

		x += dx;
		y += dy;
	}

	writeln(steps);
}
