import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm;
import std.string;

struct Instruction {
	string op;
	string x;
	string y;
}

void main() {
	auto input = File("input.txt", "r").byLine.map!(a => a.idup).array;
	auto instructions = parseInstructions(input);

	int[string] registers;
	registers["a"] = 7;
	registers["b"] = 0;
	registers["c"] = 0;
	registers["d"] = 0;

	executeInstructions(instructions, registers);

	writeln(registers["a"]);
}

Instruction[] parseInstructions(string[] input) {
	Instruction[] instructions;
	foreach (line; input) {
		auto parts = line.split(" ");
		if (parts.length == 2) {
			instructions ~= Instruction(parts[0], parts[1], "");
		} else if (parts.length == 3) {
			instructions ~= Instruction(parts[0], parts[1], parts[2]);
		}
	}
	return instructions;
}

void executeInstructions(Instruction[] instructions, int[string] registers) {
	for (int i = 0; i < instructions.length; i++) {
		auto instr = instructions[i];
		switch (instr.op) {
			case "cpy":
				if (instr.y in registers) {
					registers[instr.y] = getValue(instr.x, registers);
				}
				break;
			case "inc":
				if (instr.x in registers) {
					registers[instr.x]++;
				}
				break;
			case "dec":
				if (instr.x in registers) {
					registers[instr.x]--;
				}
				break;
			case "jnz":
				if (getValue(instr.x, registers) != 0) {
					i += getValue(instr.y, registers) - 1;
				}
				break;
			case "tgl":
				int target = i + getValue(instr.x, registers);
				if (target >= 0 && target < cast(int)instructions.length) {
					auto targetInstr = instructions[target];
					if (targetInstr.y == "") {
						if (targetInstr.op == "inc") {
							instructions[target].op = "dec";
						} else {
							instructions[target].op = "inc";
						}
					} else {
						if (targetInstr.op == "jnz") {
							instructions[target].op = "cpy";
						} else {
							instructions[target].op = "jnz";
						}
					}
				}
				break;
			default:
				break;
		}
	}
}

int getValue(string x, int[string] registers) {
	if (x in registers) {
		return registers[x];
	}
	return to!int(x);
}