import std.stdio;
import std.array;
import std.conv;

void main()
{
    string[] instructions;
    foreach (line; File("input.txt").byLine)
    {
        instructions ~= line.idup; // Use idup to create a copy of the line as a string
    }

    int accumulator = 0;
    int currentInstruction = 0;
    bool loopDetected = false;
    int[] visited = new int[](instructions.length);

    while (currentInstruction < instructions.length)
    {
        if (visited[currentInstruction] == 1)
        {
            loopDetected = true;
            break;
        }

        visited[currentInstruction] = 1;
        auto parts = instructions[currentInstruction].split(" ");
        auto op = parts[0];
        int arg = to!int(parts[1]);

        if (op == "acc")
        {
            accumulator += arg;
            currentInstruction++;
        }
        else if (op == "jmp")
        {
            currentInstruction += arg;
        }
        else if (op == "nop")
        {
            currentInstruction++;
        }
    }

    writeln(accumulator);
}