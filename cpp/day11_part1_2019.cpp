
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <unordered_map>

using namespace std;

enum PanelColor {
    Black = 0,
    White = 1
};

enum Direction {
    Up = 0,
    Right = 1,
    Down = 2,
    Left = 3
};

struct Position {
    int x, y;

    bool operator==(const Position& other) const {
        return x == other.x && y == other.y;
    }
};

namespace std {
    template <>
    struct hash<Position> {
        size_t operator()(const Position& p) const {
            return (p.x * 31 + p.y);
        }
    };
}

struct Robot {
    Position position;
    Direction direction;

    void turnAndMove(int turnDirection) {
        if (turnDirection == 0) {
            direction = static_cast<Direction>((direction + 3) % 4);
        } else {
            direction = static_cast<Direction>((direction + 1) % 4);
        }

        switch (direction) {
            case Up:
                position.y--;
                break;
            case Right:
                position.x++;
                break;
            case Down:
                position.y++;
                break;
            case Left:
                position.x--;
                break;
        }
    }
};

class Intcode {
public:
    Intcode(const vector<long long>& program) : memory(program), ip(0), halted(false) {}

    void addInput(long long input) {
        this->input.push_back(input);
    }

    void run() {
        output.clear();
        while (true) {
            long long opcode = memory[ip] % 100;
            switch (opcode) {
                case 1:
                case 2:
                case 7:
                case 8: {
                    ensureMemory(ip + 3);
                    vector<long long> params = getParams(3);
                    long long val1 = readMemory(params[0]);
                    long long val2 = readMemory(params[1]);
                    if (opcode == 1) {
                        writeMemory(params[2], val1 + val2);
                    } else if (opcode == 2) {
                        writeMemory(params[2], val1 * val2);
                    } else if ((opcode == 7 && val1 < val2) || (opcode == 8 && val1 == val2)) {
                        writeMemory(params[2], 1);
                    } else {
                        writeMemory(params[2], 0);
                    }
                    ip += 4;
                    break;
                }
                case 3:
                case 4: {
                    ensureMemory(ip + 1);
                    vector<long long> params = getParams(1);
                    if (opcode == 3) {
                        if (input.empty()) {
                            return;
                        }
                        writeMemory(params[0], input[0]);
                        input.erase(input.begin());
                    } else {
                        output.push_back(readMemory(params[0]));
                    }
                    ip += 2;
                    break;
                }
                case 5:
                case 6: {
                    ensureMemory(ip + 2);
                    vector<long long> params = getParams(2);
                    long long val = readMemory(params[0]);
                    long long target = readMemory(params[1]);
                    if ((opcode == 5 && val != 0) || (opcode == 6 && val == 0)) {
                        ip = target;
                    } else {
                        ip += 3;
                    }
                    break;
                }
                case 99:
                    halted = true;
                    return;
                default:
                    throw runtime_error("unknown opcode: " + to_string(opcode));
            }
        }
    }

    const vector<long long>& getOutput() const {
        return output;
    }

    bool isHalted() const {
        return halted;
    }

private:
    vector<long long> memory;
    long long ip;
    vector<long long> input;
    vector<long long> output;
    bool halted;
    long long relative_base = 0;

    long long readMemory(long long address) {
        ensureMemory(address);
        return memory[address];
    }

    void writeMemory(long long address, long long value) {
        ensureMemory(address);
        memory[address] = value;
    }

    void ensureMemory(long long address) {
        if (address >= memory.size()) {
            memory.resize(address + 1, 0);
        }
    }

    vector<long long> getParams(int count) {
        long long paramModes = memory[ip] / 100;
        vector<long long> params(count);
        for (int i = 0; i < count; i++) {
            long long mode = paramModes % 10;
            if (mode == 0) {
                params[i] = memory[ip + i + 1];
            } else if (mode == 1) {
                params[i] = ip + i + 1;
            }
            paramModes /= 10;
        }
        return params;
    }
};

int main() {
    ifstream inputFile("input.txt");
    string line;
    getline(inputFile, line);
    inputFile.close();

    stringstream ss(line);
    string token;
    vector<long long> program;
    while (getline(ss, token, ',')) {
        program.push_back(stoll(token));
    }

    unordered_map<Position, PanelColor> grid;
    Robot robot = {{0, 0}, Up};
    Intcode intcode(program);

    while (!intcode.isHalted()) {
        PanelColor currentColor = grid.count(robot.position) ? grid[robot.position] : Black;
        intcode.addInput(static_cast<long long>(currentColor));
        intcode.run();
        const vector<long long>& outputs = intcode.getOutput();

        if (outputs.size() == 2) {
            grid[robot.position] = static_cast<PanelColor>(outputs[0]);
            robot.turnAndMove(static_cast<int>(outputs[1]));
        }
    }

    cout << grid.size() << endl;

    return 0;
}
