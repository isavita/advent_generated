
import std.stdio;
import std.algorithm;
import std.conv;
import std.file;
import std.array; // Added import statement for std.array

void main()
{
	auto file = File("input.txt", "r");

	int[] positions;
	foreach (line; file.byLine())
	{
		auto numbers = line.split(",");
		foreach (num_str; numbers)
		{
			auto num = to!int(num_str);
			positions ~= num;
		}
	}

	positions.sort();

	int min_fuel = int.max;
	foreach (i; positions[0] .. positions[$-1])
	{
		int fuel = 0;
		foreach (pos; positions)
		{
			fuel += calculateFuel(pos, i);
		}
		if (fuel < min_fuel)
		{
			min_fuel = fuel;
		}
	}
	writeln(min_fuel);
}

int calculateFuel(int currentPosition, int newPosition)
{
	return abs(currentPosition - newPosition);
}

int abs(int n)
{
	return n < 0 ? -n : n;
}
