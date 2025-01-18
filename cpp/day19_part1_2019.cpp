
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
    ifstream inputFile;

    VM(const string& filename) : ip(0), relativeBase(0) {
        load(filename);
    }

    void load(const string& filename) {
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

    long long getParamAddress(long long pos, long long mode) {
        switch (mode) {
            case 0: return code[pos];
            case 1: return pos;
            case 2: return relativeBase + code[pos];
        }
        return -1;
    }

    vector<long long> getParamsAddresses(long long pos, long long cmd, int arity) {
        vector<long long> modes(arity);
        long long modeSection = cmd / 100;
        for (int i = 0; i < arity; ++i) {
            modes[i] = (modeSection / (long long)pow(10, i)) % 10;
        }
        vector<long long> results(arity);
        for (int i = 0; i < arity; ++i) {
            results[i] = getParamAddress(pos + i + 1, modes[i]);
        }
        return results;
    }

    long long run(long long x, long long y) {
        ip = 0;
        relativeBase = 0;
        map<long long, long long> currentCode = code;
        bool x_inputted = false;
        while (true) {
            long long cmd = currentCode[ip];
            long long opcode = cmd % 100;
            int arity;

            switch (opcode) {
                case 1: {
                    arity = 3;
                    vector<long long> params = getParamsAddresses(ip, cmd, arity);
                    currentCode[params[2]] = currentCode[params[0]] + currentCode[params[1]];
                    break;
                }
                case 2: {
                    arity = 3;
                    vector<long long> params = getParamsAddresses(ip, cmd, arity);
                    currentCode[params[2]] = currentCode[params[0]] * currentCode[params[1]];
                    break;
                }
                case 3: {
                    arity = 1;
                    vector<long long> params = getParamsAddresses(ip, cmd, arity);
                    if (!x_inputted) {
                        currentCode[params[0]] = x;
                        x_inputted = true;
                    } else {
                        currentCode[params[0]] = y;
                    }
                    
                    break;
                }
                case 4: {
                    arity = 1;
                    vector<long long> params = getParamsAddresses(ip, cmd, arity);
                    return currentCode[params[0]];
                }
                case 5: {
                    arity = 2;
                    vector<long long> params = getParamsAddresses(ip, cmd, arity);
                    if (currentCode[params[0]] != 0) {
                        ip = currentCode[params[1]];
                        continue;
                    }
                    break;
                }
                case 6: {
                    arity = 2;
                    vector<long long> params = getParamsAddresses(ip, cmd, arity);
                    if (currentCode[params[0]] == 0) {
                        ip = currentCode[params[1]];
                        continue;
                    }
                    break;
                }
                case 7: {
                    arity = 3;
                    vector<long long> params = getParamsAddresses(ip, cmd, arity);
                    currentCode[params[2]] = (currentCode[params[0]] < currentCode[params[1]]) ? 1 : 0;
                    break;
                }
                case 8: {
                    arity = 3;
                    vector<long long> params = getParamsAddresses(ip, cmd, arity);
                    currentCode[params[2]] = (currentCode[params[0]] == currentCode[params[1]]) ? 1 : 0;
                    break;
                }
                case 9: {
                    arity = 1;
                    vector<long long> params = getParamsAddresses(ip, cmd, arity);
                    relativeBase += currentCode[params[0]];
                    break;
                }
                case 99:
                    return -1;
                default:
                    return -1;
            }
            ip += arity + 1;
        }
    }
};

int main() {
    VM vm("input.txt");
    int sum = 0;
    for (int y = 0; y < 50; ++y) {
        for (int x = 0; x < 50; ++x) {
            if (vm.run(x, y) == 1) {
                sum++;
            }
        }
    }
    cout << sum << endl;
    return 0;
}
