#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <sstream>

using namespace std;

map<string, string> monkeys;

long long evaluateMonkey(string monkey) {
    if (monkeys[monkey][0] >= '0' && monkeys[monkey][0] <= '9') {
        return stol(monkeys[monkey]);
    }

    stringstream ss(monkeys[monkey]);
    string op1, op, op2;
    ss >> op1 >> op >> op2;

    long long val1 = evaluateMonkey(op1);
    long long val2 = evaluateMonkey(op2);

    if (op == "+") {
        return val1 + val2;
    } else if (op == "-") {
        return val1 - val2;
    } else if (op == "*") {
        return val1 * val2;
    } else if (op == "/") {
        return val1 / val2;
    }

    return 0;
}

int main() {
    ifstream input("input.txt");
    string line;

    while (getline(input, line)) {
        size_t colonPos = line.find(":");
        string monkey = line.substr(0, colonPos);
        string job = line.substr(colonPos + 2);
        monkeys[monkey] = job;
    }

    long long rootValue = evaluateMonkey("root");
    cout << "The monkey named root will yell: " << rootValue << endl;

    return 0;
}