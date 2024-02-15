
import std.file;
import std.conv;
import std.array;
import std.stdio;

void main()
{
    string input = cast(string) read("input.txt");
    string[] lines = input.split("\n");

    int ipRegister = lines[0].split(" ")[1].to!int;
    lines = lines[1..$];

    int[] registers = [0, 0, 0, 0, 0, 0];

    while (registers[ipRegister] < lines.length)
    {
        string line = lines[registers[ipRegister]];
        string[] parts = line.split(" ");
        string op = parts[0];
        int a = parts[1].to!int;
        int b = parts[2].to!int;
        int c = parts[3].to!int;

        switch (op)
        {
            case "addr": registers[c] = registers[a] + registers[b]; break;
            case "addi": registers[c] = registers[a] + b; break;
            case "mulr": registers[c] = registers[a] * registers[b]; break;
            case "muli": registers[c] = registers[a] * b; break;
            case "banr": registers[c] = registers[a] & registers[b]; break;
            case "bani": registers[c] = registers[a] & b; break;
            case "borr": registers[c] = registers[a] | registers[b]; break;
            case "bori": registers[c] = registers[a] | b; break;
            case "setr": registers[c] = registers[a]; break;
            case "seti": registers[c] = a; break;
            case "gtir": registers[c] = a > registers[b] ? 1 : 0; break;
            case "gtri": registers[c] = registers[a] > b ? 1 : 0; break;
            case "gtrr": registers[c] = registers[a] > registers[b] ? 1 : 0; break;
            case "eqir": registers[c] = a == registers[b] ? 1 : 0; break;
            case "eqri": registers[c] = registers[a] == b ? 1 : 0; break;
            case "eqrr": registers[c] = registers[a] == registers[b] ? 1 : 0; break;
            default: assert(0);
        }

        registers[ipRegister]++;
    }

    writeln(registers[0]);
}
