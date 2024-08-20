import std.stdio;
import std.file;
import std.conv;
import std.string;
import std.algorithm;
import std.array;

void main()
{
    auto file = File("input.txt", "r");
    string[] lines;
    foreach (line; file.byLine())
    {
        lines ~= line.idup;
    }
    file.close();

    int[string] registers;
    foreach (line; lines)
    {
        auto parts = line.split(" ");
        auto reg = parts[0];
        auto op = parts[1];
        auto amount = to!int(parts[2]);
        auto condReg = parts[4];
        auto condOp = parts[5];
        auto condVal = to!int(parts[6]);

        if (condReg !in registers)
        {
            registers[condReg] = 0;
        }

        bool conditionMet = false;
        switch (condOp)
        {
            case ">":
                conditionMet = registers[condReg] > condVal;
                break;
            case "<":
                conditionMet = registers[condReg] < condVal;
                break;
            case ">=":
                conditionMet = registers[condReg] >= condVal;
                break;
            case "<=":
                conditionMet = registers[condReg] <= condVal;
                break;
            case "==":
                conditionMet = registers[condReg] == condVal;
                break;
            case "!=":
                conditionMet = registers[condReg] != condVal;
                break;
            default:
                assert(0, "Unexpected condition operator");
        }

        if (conditionMet)
        {
            if (reg !in registers)
            {
                registers[reg] = 0;
            }

            if (op == "inc")
            {
                registers[reg] += amount;
            }
            else if (op == "dec")
            {
                registers[reg] -= amount;
            }
        }
    }

    int maxVal = registers.byValue.maxElement;
    writeln(maxVal);
}