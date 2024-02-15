
import std.stdio;
import std.file;
import std.conv;
import std.string;

void main()
{
    string[] instructions;
    foreach (line; File("input.txt").byLine)
    {
        instructions ~= line.idup; // idup creates a copy of the string
    }

    int[string] registers = ["a": 0, "b": 0, "c": 0, "d": 0];
    executeInstructions(instructions, registers);

    writeln(registers["a"]);
}

void executeInstructions(string[] instructions, int[string] registers)
{
    for (int i = 0; i < instructions.length;)
    {
        auto parts = split(instructions[i]);
        switch (parts[0])
        {
            case "cpy":
                int val = getValue(parts[1], registers);
                registers[parts[2]] = val;
                i++;
                break;
            case "inc":
                registers[parts[1]]++;
                i++;
                break;
            case "dec":
                registers[parts[1]]--;
                i++;
                break;
            case "jnz":
                int val = getValue(parts[1], registers);
                if (val != 0)
                {
                    int jump = parts[2].to!int;
                    i += jump;
                }
                else
                {
                    i++;
                }
                break;
            default:
                assert(0); // Default case to handle any unhandled instructions
        }
    }
}

int getValue(string s, int[string] registers)
{
    try
    {
        return to!int(s);
    }
    catch (Throwable)
    {
        return registers[s];
    }
}
