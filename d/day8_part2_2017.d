
import std.stdio, std.file, std.string, std.conv, std.algorithm;

void main() {
    int[string] registers;
    int maxValueEver = 0;

    foreach (line; readText("input.txt").strip().splitLines()) {
        auto p = line.split();
        auto reg = p[0];
        auto op = p[1];
        auto val = p[2].to!int;
        auto condReg = p[4];
        auto condOp = p[5];
        auto condVal = p[6].to!int;

        bool conditionMet;
        switch (condOp) {
            case "==": conditionMet = (registers.get(condReg, 0) == condVal); break;
            case "!=": conditionMet = (registers.get(condReg, 0) != condVal); break;
            case ">":  conditionMet = (registers.get(condReg, 0) > condVal); break;
            case "<":  conditionMet = (registers.get(condReg, 0) < condVal); break;
            case ">=": conditionMet = (registers.get(condReg, 0) >= condVal); break;
            case "<=": conditionMet = (registers.get(condReg, 0) <= condVal); break;
            default:   assert(0);
        }

        if (conditionMet) {
            if (op == "inc") registers[reg] += val;
            else registers[reg] -= val;
            maxValueEver = max(maxValueEver, registers[reg]);
        }
    }

    writeln(registers.values.empty ? 0 : registers.values.maxElement);
    writeln(maxValueEver);
}
