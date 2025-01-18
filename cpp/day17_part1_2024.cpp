
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

int getComboVal(int op, int A, int B, int C) {
    if (op <= 3) return op;
    if (op == 4) return A;
    if (op == 5) return B;
    if (op == 6) return C;
    return -1;
}

int main() {
    ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        cerr << "Error opening input.txt" << endl;
        return 1;
    }

    int A = 0, B = 0, C = 0;
    vector<int> program;
    string line;

    while (getline(inputFile, line)) {
        line.erase(remove(line.begin(), line.end(), '\r'), line.end());
        if (line.empty()) continue;

        if (line.rfind("Register A:", 0) == 0) {
            A = stoi(line.substr(12));
        } else if (line.rfind("Register B:", 0) == 0) {
            B = stoi(line.substr(12));
        } else if (line.rfind("Register C:", 0) == 0) {
            C = stoi(line.substr(12));
        } else if (line.rfind("Program:", 0) == 0) {
            stringstream ss(line.substr(9));
            string numStr;
            while (getline(ss, numStr, ',')) {
                program.push_back(stoi(numStr));
            }
        }
    }

    vector<string> outputVals;
    int ip = 0;
    while (ip < program.size()) {
        int opcode = program[ip];
        if (ip + 1 >= program.size()) break;
        int operand = program[ip + 1];

        switch (opcode) {
            case 0: {
                int den = getComboVal(operand, A, B, C);
                if (den == 0) {
                    A = 0;
                } else {
                    A /= (1 << den);
                }
                ip += 2;
                break;
            }
            case 1:
                B ^= operand;
                ip += 2;
                break;
            case 2:
                B = getComboVal(operand, A, B, C) % 8;
                ip += 2;
                break;
            case 3:
                if (A != 0) {
                    ip = operand;
                } else {
                    ip += 2;
                }
                break;
            case 4:
                B ^= C;
                ip += 2;
                break;
            case 5:
                outputVals.push_back(to_string(getComboVal(operand, A, B, C) % 8));
                ip += 2;
                break;
            case 6: {
                int den = getComboVal(operand, A, B, C);
                B = A / (1 << den);
                ip += 2;
                break;
            }
            case 7: {
                int den = getComboVal(operand, A, B, C);
                C = A / (1 << den);
                ip += 2;
                break;
            }
            default:
                break;
        }
    }

    for (size_t i = 0; i < outputVals.size(); ++i) {
        cout << outputVals[i];
        if (i < outputVals.size() - 1) {
            cout << ",";
        }
    }
    cout << endl;

    return 0;
}
