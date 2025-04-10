
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>

using namespace std;

int main() {
    ifstream file("input.txt");
    string line;

    getline(file, line);
    int ip_register = stoi(line.substr(4));

    vector<string> instructions;
    while (getline(file, line)) {
        instructions.push_back(line);
    }

    vector<int> registers(6, 0);

    while (0 <= registers[ip_register] && registers[ip_register] < instructions.size()) {
        int instruction_pointer = registers[ip_register];
        string instruction = instructions[instruction_pointer];

        stringstream ss(instruction);
        string opcode;
        int A, B, C;
        ss >> opcode >> A >> B >> C;

        if (opcode == "addr") {
            registers[C] = registers[A] + registers[B];
        } else if (opcode == "addi") {
            registers[C] = registers[A] + B;
        } else if (opcode == "mulr") {
            registers[C] = registers[A] * registers[B];
        } else if (opcode == "muli") {
            registers[C] = registers[A] * B;
        } else if (opcode == "banr") {
            registers[C] = registers[A] & registers[B];
        } else if (opcode == "bani") {
            registers[C] = registers[A] & B;
        } else if (opcode == "borr") {
            registers[C] = registers[A] | registers[B];
        } else if (opcode == "bori") {
            registers[C] = registers[A] | B;
        } else if (opcode == "setr") {
            registers[C] = registers[A];
        } else if (opcode == "seti") {
            registers[C] = A;
        } else if (opcode == "gtir") {
            registers[C] = (A > registers[B]) ? 1 : 0;
        } else if (opcode == "gtri") {
            registers[C] = (registers[A] > B) ? 1 : 0;
        } else if (opcode == "gtrr") {
            registers[C] = (registers[A] > registers[B]) ? 1 : 0;
        } else if (opcode == "eqir") {
            registers[C] = (A == registers[B]) ? 1 : 0;
        } else if (opcode == "eqri") {
            registers[C] = (registers[A] == B) ? 1 : 0;
        } else if (opcode == "eqrr") {
            registers[C] = (registers[A] == registers[B]) ? 1 : 0;
        }

        registers[ip_register]++;
    }

    cout << registers[0] << endl;

    return 0;
}
