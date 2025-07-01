
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.algorithm;
import std.array;
import std.regex;

struct Op {
    char action;
    char a, b;
    int[] candidates;
}

Op[] opcodes;

static this() {
    opcodes = [
        Op('+', 'r', 'r'), Op('+', 'r', 'v'),
        Op('*', 'r', 'r'), Op('*', 'r', 'v'),
        Op('&', 'r', 'r'), Op('&', 'r', 'v'),
        Op('|', 'r', 'r'), Op('|', 'r', 'v'),
        Op('a', 'r', 'r'), Op('a', 'v', 'r'),
        Op('>', 'v', 'r'), Op('>', 'r', 'v'), Op('>', 'r', 'r'),
        Op('=', 'v', 'r'), Op('=', 'r', 'v'), Op('=', 'r', 'r'),
    ];
}

void runOp(const Op op, ref int[4] regs, const int[] instr) {
    auto A = op.a == 'r' ? regs[instr[1]] : instr[1];
    auto B = op.b == 'r' ? regs[instr[2]] : instr[2];
    switch (op.action) {
        case '+': regs[instr[3]] = A + B; break;
        case '*': regs[instr[3]] = A * B; break;
        case '&': regs[instr[3]] = A & B; break;
        case '|': regs[instr[3]] = A | B; break;
        case 'a': regs[instr[3]] = A; break;
        case '>': regs[instr[3]] = A > B ? 1 : 0; break;
        case '=': regs[instr[3]] = A == B ? 1 : 0; break;
        default: assert(0);
    }
}

int[] parseNums(string s) {
    auto parts = s.split(regex(`[^0-9-]+`));
    if (parts.length > 0 && parts[0].length == 0) {
        parts = parts[1..$];
    }
    return parts.filter!(a => a.length > 0).map!(to!int).array;
}

void main() {
    auto lines = readText("input.txt").splitLines();
    int lineIdx = 0;

    while (lineIdx + 2 < lines.length && lines[lineIdx].startsWith("Before")) {
        int[4] before, after;
        before[] = parseNums(lines[lineIdx])[];
        auto instr = parseNums(lines[lineIdx + 1]);
        after[] = parseNums(lines[lineIdx + 2])[];

        foreach (i, ref op; opcodes) {
            auto regsCopy = before;
            runOp(op, regsCopy, instr);
            if (regsCopy == after) {
                if (!op.candidates.canFind(instr[0])) {
                    op.candidates ~= instr[0];
                }
            }
        }
        lineIdx += 4;
    }

    Op[int] finalOpcodes;
    while (finalOpcodes.length < 16) {
        foreach (i, ref op; opcodes) {
            if (op.candidates.length == 1) {
                int code = op.candidates[0];
                if (code in finalOpcodes) continue;
                
                finalOpcodes[code] = op;
                
                foreach (ref otherOp; opcodes) {
                    otherOp.candidates = otherOp.candidates.filter!(c => c != code).array;
                }
                break;
            }
        }
    }

    while (lineIdx < lines.length && lines[lineIdx].strip.length == 0) {
        lineIdx++;
    }

    int[4] r = [0, 0, 0, 0];
    for (; lineIdx < lines.length; lineIdx++) {
        auto line = lines[lineIdx].strip;
        if (line.length == 0) continue;
        auto instr = parseNums(line);
        runOp(finalOpcodes[instr[0]], r, instr);
    }

    writeln(r[0]);
}
