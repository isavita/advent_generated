import std.stdio;
import std.file;
import std.conv;
import std.array;

struct Instruction {
	string op;
	int arg;
}

Instruction[] parseInstructions(string data) {
	Instruction[] instructions;
	auto lines = data.split("\n");
	foreach (line; lines) {
		auto parts = line.split(" ");
		auto arg = to!int(parts[1]);
		instructions ~= Instruction(parts[0], arg);
	}
	return instructions;
}

bool runProgram(Instruction[] instructions, ref int accumulator) {
	bool[int] visited;
	int pc = 0;
	accumulator = 0;
	while (pc < instructions.length) {
		if (pc in visited) {
			return false;
		}
		visited[pc] = true;
		auto instr = instructions[pc];
		switch (instr.op) {
			case "acc":
				accumulator += instr.arg;
				pc++;
				break;
			case "jmp":
				pc += instr.arg;
				break;
			case "nop":
				pc++;
				break;
			default:
				assert(0);
		}
	}
	return true;
}

void main() {
	auto data = cast(string)read("input.txt");
	auto instructions = parseInstructions(data);

	foreach (i, instr; instructions) {
		if (instr.op == "jmp" || instr.op == "nop") {
			auto modified = instructions.dup;
			modified[i].op = instr.op == "jmp" ? "nop" : "jmp";
			int accumulator;
			if (runProgram(modified, accumulator)) {
				writeln(accumulator);
				return;
			}
		}
	}
}