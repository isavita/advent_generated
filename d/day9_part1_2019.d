import std.stdio;
import std.file;
import std.conv;
import std.array;
import std.algorithm; // Added import for map

void main()
{
    string input = cast(string) read("input.txt");
    auto program = input.split(",").map!(to!long).array;

    long[] memory;
    memory.length = 10000;
    foreach (i, v; program)
    {
        memory[i] = v;
    }

    long relativeBase = 0;
    long i = 0;
    while (i < memory.length)
    {
        long opcode = memory[i] % 100;
        long mode1 = (memory[i] / 100) % 10;
        long mode2 = (memory[i] / 1000) % 10;
        long mode3 = (memory[i] / 10000) % 10;

        if (opcode == 99)
        {
            break;
        }
        else if (opcode == 1 || opcode == 2)
        {
            long val1 = getValue(memory, i + 1, mode1, relativeBase);
            long val2 = getValue(memory, i + 2, mode2, relativeBase);
            long addr3 = getAddress(memory, i + 3, mode3, relativeBase);

            if (opcode == 1)
            {
                memory[addr3] = val1 + val2;
            }
            else
            {
                memory[addr3] = val1 * val2;
            }
            i += 4;
        }
        else if (opcode == 3)
        {
            long addr1 = getAddress(memory, i + 1, mode1, relativeBase);
            memory[addr1] = 1;
            i += 2;
        }
        else if (opcode == 4)
        {
            long val1 = getValue(memory, i + 1, mode1, relativeBase);
            writeln(val1);
            i += 2;
        }
        else if (opcode == 5 || opcode == 6)
        {
            long val1 = getValue(memory, i + 1, mode1, relativeBase);
            long val2 = getValue(memory, i + 2, mode2, relativeBase);

            if ((opcode == 5 && val1 != 0) || (opcode == 6 && val1 == 0))
            {
                i = val2;
            }
            else
            {
                i += 3;
            }
        }
        else if (opcode == 7 || opcode == 8)
        {
            long val1 = getValue(memory, i + 1, mode1, relativeBase);
            long val2 = getValue(memory, i + 2, mode2, relativeBase);
            long addr3 = getAddress(memory, i + 3, mode3, relativeBase);

            if ((opcode == 7 && val1 < val2) || (opcode == 8 && val1 == val2))
            {
                memory[addr3] = 1;
            }
            else
            {
                memory[addr3] = 0;
            }
            i += 4;
        }
        else if (opcode == 9)
        {
            long val1 = getValue(memory, i + 1, mode1, relativeBase);
            relativeBase += val1;
            i += 2;
        }
        else
        {
            writeln("Error: Unknown opcode encountered: ", opcode);
            break;
        }
    }
}

long getValue(long[] memory, long index, long mode, long relativeBase)
{
    if (mode == 0)
    {
        return memory[memory[index]];
    }
    else if (mode == 1)
    {
        return memory[index];
    }
    else if (mode == 2)
    {
        return memory[relativeBase + memory[index]];
    }
    return 0;
}

long getAddress(long[] memory, long index, long mode, long relativeBase)
{
    if (mode == 0)
    {
        return memory[index];
    }
    else if (mode == 2)
    {
        return relativeBase + memory[index];
    }
    return 0;
}