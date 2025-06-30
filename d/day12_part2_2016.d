
import std.stdio, std.file, std.string, std.conv;

void main() {
    auto instructions = readText("input.txt").splitLines();
    long[4] registers = [0, 0, 1, 0]; // a, b, c, d

    size_t i = 0;
    while (i < instructions.length) {
        auto parts = instructions[i].split();
        final switch (parts[0]) {
            case "cpy":
                auto val = (parts[1][0] >= 'a')
                    ? registers[parts[1][0] - 'a']
                    : parts[1].to!long;
                registers[parts[2][0] - 'a'] = val;
                i++;
                break;
            case "inc":
                registers[parts[1][0] - 'a']++;
                i++;
                break;
            case "dec":
                registers[parts[1][0] - 'a']--;
                i++;
                break;
            case "jnz":
                auto val = (parts[1][0] >= 'a')
                    ? registers[parts[1][0] - 'a']
                    : parts[1].to!long;
                if (val != 0) {
                    i += parts[2].to!long;
                } else {
                    i++;
                }
                break;
        }
    }
    writeln(registers[0]);
}
