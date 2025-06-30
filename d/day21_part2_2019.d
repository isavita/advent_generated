
import std.stdio;
import std.file;
import std.string;
import std.conv;
import std.array;

struct VM {
    long[long] code;
    long ip;
    long[] input;
    long[] output;
    long relativeBase;

    static immutable long[] p10 = [100, 1000, 10000];

    this(string filename) {
        auto text = readText(filename).strip();
        foreach (i, s; text.split(',')) {
            code[i] = s.to!long;
        }
    }

    private long getParam(int index) {
        long mode = (code.get(ip, 0) / p10[index - 1]) % 10;
        long val = code.get(ip + index, 0);
        final switch (mode) {
            case 0: return code.get(val, 0);
            case 1: return val;
            case 2: return code.get(relativeBase + val, 0);
        }
    }

    private long getAddress(int index) {
        long mode = (code.get(ip, 0) / p10[index - 1]) % 10;
        long val = code.get(ip + index, 0);
        if (mode == 2) return relativeBase + val;
        return val;
    }

    void sendString(string s) {
        foreach (c; s) {
            input ~= c;
        }
        input ~= 10;
    }

    void run() {
        while (true) {
            long opcode = code.get(ip, 0) % 100;
            switch (opcode) {
                case 1:
                    code[getAddress(3)] = getParam(1) + getParam(2);
                    ip += 4;
                    break;
                case 2:
                    code[getAddress(3)] = getParam(1) * getParam(2);
                    ip += 4;
                    break;
                case 3:
                    code[getAddress(1)] = input[0];
                    input = input[1 .. $];
                    ip += 2;
                    break;
                case 4:
                    output ~= getParam(1);
                    ip += 2;
                    break;
                case 5:
                    if (getParam(1) != 0) ip = getParam(2);
                    else ip += 3;
                    break;
                case 6:
                    if (getParam(1) == 0) ip = getParam(2);
                    else ip += 3;
                    break;
                case 7:
                    code[getAddress(3)] = (getParam(1) < getParam(2)) ? 1 : 0;
                    ip += 4;
                    break;
                case 8:
                    code[getAddress(3)] = (getParam(1) == getParam(2)) ? 1 : 0;
                    ip += 4;
                    break;
                case 9:
                    relativeBase += getParam(1);
                    ip += 2;
                    break;
                case 99:
                    return;
                default:
                    assert(0, "Unknown opcode");
            }
        }
    }
}

void main() {
    auto vm = VM("input.txt");
    string[] instructions = [
        "NOT A J",
        "NOT B T",
        "OR T J",
        "NOT C T",
        "OR T J",
        "AND D J",
        "NOT A T",
        "AND A T",
        "OR E T",
        "OR H T",
        "AND T J",
        "RUN",
    ];

    foreach (instruction; instructions) {
        vm.sendString(instruction);
    }

    vm.run();
    writeln(vm.output[$ - 1]);
}
