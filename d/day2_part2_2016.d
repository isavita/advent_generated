
import std.stdio;
import std.file;
import std.array;
import std.string;

void main()
{
	auto file = File("input.txt", "r");
	auto lines = file.byLineCopy();
	auto instructions = lines.array();

	auto code = getBathroomCode(instructions);
	writeln(code);
}

string getBathroomCode(string[] instructions)
{
	auto keypad = [
		"1": ['D': "3"],
		"2": ['R': "3", 'D': "6"],
		"3": ['U': "1", 'R': "4", 'D': "7", 'L': "2"],
		"4": ['L': "3", 'D': "8"],
		"5": ['R': "6"],
		"6": ['U': "2", 'R': "7", 'D': "A", 'L': "5"],
		"7": ['U': "3", 'R': "8", 'D': "B", 'L': "6"],
		"8": ['U': "4", 'R': "9", 'D': "C", 'L': "7"],
		"9": ['L': "8"],
		"A": ['U': "6", 'R': "B"],
		"B": ['U': "7", 'R': "C", 'D': "D", 'L': "A"],
		"C": ['U': "8", 'L': "B"],
		"D": ['U': "B"]
	];

	string position = "5";
	string code = "";

	foreach (instruction; instructions)
	{
		foreach (move; instruction)
		{
			auto nextPos = keypad[position].get(move, position);
			position = nextPos;
		}
		code ~= position;
	}

	return code;
}
