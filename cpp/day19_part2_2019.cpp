
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>

using namespace std;

class VM {
public:
    map<long long, long long> code;
    long long ip;
    long long relativeBase;
    vector<long long> input;
    vector<long long> output;
    int inputIndex;

    VM(const string& filename) : ip(0), relativeBase(0), inputIndex(0) {
        Load(filename);
    }

    void Load(const string& filename) {
        ifstream file(filename);
        string line;
        getline(file, line);
        stringstream ss(line);
        string token;
        long long i = 0;
        while (getline(ss, token, ',')) {
            code[i++] = stoll(token);
        }
    }

    void Run() {
        long long arity;
        while (true) {
            long long cmd = code[ip];
            long long opcode = cmd % 100;

            switch (opcode) {
                case 1:
                    arity = 3;
                    code[getParamAddress(ip, cmd, 3, 2)] = code[getParamAddress(ip, cmd, 3, 0)] + code[getParamAddress(ip, cmd, 3, 1)];
                    break;
                case 2:
                    arity = 3;
                    code[getParamAddress(ip, cmd, 3, 2)] = code[getParamAddress(ip, cmd, 3, 0)] * code[getParamAddress(ip, cmd, 3, 1)];
                    break;
                case 3:
                    arity = 1;
                    code[getParamAddress(ip, cmd, 1, 0)] = input[inputIndex++];
                    break;
                case 4:
                    arity = 1;
                    output.push_back(code[getParamAddress(ip, cmd, 1, 0)]);
                    break;
                case 5:
                    arity = 2;
                    if (code[getParamAddress(ip, cmd, 2, 0)] != 0) {
                        ip = code[getParamAddress(ip, cmd, 2, 1)];
                        continue;
                    }
                    break;
                case 6:
                    arity = 2;
                    if (code[getParamAddress(ip, cmd, 2, 0)] == 0) {
                        ip = code[getParamAddress(ip, cmd, 2, 1)];
                        continue;
                    }
                    break;
                case 7:
                    arity = 3;
                    code[getParamAddress(ip, cmd, 3, 2)] = (code[getParamAddress(ip, cmd, 3, 0)] < code[getParamAddress(ip, cmd, 3, 1)]) ? 1 : 0;
                    break;
                case 8:
                    arity = 3;
                    code[getParamAddress(ip, cmd, 3, 2)] = (code[getParamAddress(ip, cmd, 3, 0)] == code[getParamAddress(ip, cmd, 3, 1)]) ? 1 : 0;
                    break;
                case 9:
                    arity = 1;
                    relativeBase += code[getParamAddress(ip, cmd, 1, 0)];
                    break;
                case 99:
                    return;
                default:
                    cerr << "Invalid opcode: " << opcode << endl;
                    return;
            }
            ip += arity + 1;
        }
    }

    long long getParamAddress(long long pos, long long cmd, long long arity, long long paramIndex) {
        long long mode = (cmd / (long long)pow(10, paramIndex + 2)) % 10;
        switch (mode) {
            case 0:
                return code[pos + paramIndex + 1];
            case 1:
                return pos + paramIndex + 1;
            case 2:
                return relativeBase + code[pos + paramIndex + 1];
            default:
                cerr << "Invalid mode: " << mode << endl;
                return -1;
        }
    }
};

bool Beam(int x, int y) {
    VM vm("input.txt");
    vm.input.push_back(x);
    vm.input.push_back(y);
    vm.Run();
    return vm.output[0] == 1;
}

int main() {
    int y = 20;
    int x = 0;

    while (true) {
        if (!Beam(x, y)) {
            x++;
            continue;
        }
        if (!Beam(x + 99, y)) {
            y++;
            continue;
        }
        if (!Beam(x, y + 99)) {
            x++;
            continue;
        }
        cout << x * 10000 + y << endl;
        return 0;
    }
}
