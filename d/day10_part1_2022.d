import std.stdio;
import std.file;
import std.conv;
import std.array; // Added import for split

void main()
{
    string input = cast(string) read("input.txt");
    string[] instructions = input.split("\n");

    int cycle = 0;
    int x = 1;
    int sumSignalStrengths = 0;

    foreach (instruction; instructions)
    {
        string[] parts = instruction.split(" ");
        string opcode = parts[0];

        if (opcode == "noop")
        {
            cycle++;
            if (cycle == 20 || (cycle - 20) % 40 == 0)
            {
                sumSignalStrengths += cycle * x;
            }
        }
        else if (opcode == "addx")
        {
            int value = to!int(parts[1]);

            cycle++;
            if (cycle == 20 || (cycle - 20) % 40 == 0)
            {
                sumSignalStrengths += cycle * x;
            }

            cycle++;
            if (cycle == 20 || (cycle - 20) % 40 == 0)
            {
                sumSignalStrengths += cycle * x;
            }

            x += value;
        }
    }

    writeln(sumSignalStrengths);
}