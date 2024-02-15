import std.stdio;
import std.conv;
import std.file;

void main()
{
	auto freqChanges = readInput();
	int freq = 0;
	foreach(change; freqChanges)
	{
		freq += parseChange(change);
	}
	writeln(freq);
}

string[] readInput()
{
	string[] lines;
	auto file = File("input.txt", "r");
	foreach(line; file.byLine())
	{
		lines ~= to!string(line);
	}
	return lines;
}

int parseChange(string change)
{
	auto tuple = parseSignNum(change);
	return tuple[0] * tuple[1];
}

int[] parseSignNum(string change)
{
	int sign = 1;
	if (change[0] == '-')
	{
		sign = -1;
		change = change[1..$];
	}
	int num = to!int(change);
	return [sign, num];
}