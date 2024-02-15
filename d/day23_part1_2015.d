import std.stdio;
import std.file;
import std.conv;
import std.array; // Added import for std.array

void main()
{
    string data = cast(string) read("input.txt");
    string[] instructions = data.split("\n");

    int[string] registers = ["a": 0, "b": 0];

    for (int i = 0; i < instructions.length; i++)
    {
        string[] parts = instructions[i].split(" ");

        switch (parts[0])
        {
            case "hlf":
                registers[parts[1]] /= 2;
                break;
            case "tpl":
                registers[parts[1]] *= 3;
                break;
            case "inc":
                registers[parts[1]]++;
                break;
            case "jmp":
                int offset = to!int(parts[1]);
                i += offset - 1;
                break;
            case "jie":
                if (registers[parts[1][0..$-1]] % 2 == 0)
                {
                    int offset = to!int(parts[2]);
                    i += offset - 1;
                }
                break;
            case "jio":
                if (registers[parts[1][0..$-1]] == 1)
                {
                    int offset = to!int(parts[2]);
                    i += offset - 1;
                }
                break;
            default:
                writeln("Unknown instruction: ", parts[0]);
                break;
        }
    }

    writeln(registers["b"]);
}