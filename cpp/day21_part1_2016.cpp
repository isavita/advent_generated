#include <iostream>
#include <fstream>
#include <vector>
#include <string>

std::string applyOperation(std::string op, std::string password) {
    std::vector<std::string> fields;
    size_t start = 0;
    size_t end = op.find(" ");
    while (end != std::string::npos) {
        fields.push_back(op.substr(start, end - start));
        start = end + 1;
        end = op.find(" ", start);
    }
    fields.push_back(op.substr(start));

    if (fields[0] == "swap") {
        if (fields[1] == "position") {
            int x = fields[2][0] - '0';
            int y = fields[5][0] - '0';
            std::swap(password[x], password[y]);
        } else if (fields[1] == "letter") {
            char x = fields[2][0];
            char y = fields[5][0];
            for (char &c : password) {
                if (c == x) {
                    c = y;
                } else if (c == y) {
                    c = x;
                }
            }
        }
    } else if (fields[0] == "rotate") {
        if (fields[1] == "left") {
            int steps = fields[2][0] - '0';
            steps = steps % password.size();
            std::rotate(password.begin(), password.begin() + steps, password.end());
        } else if (fields[1] == "right") {
            int steps = fields[2][0] - '0';
            steps = steps % password.size();
            std::rotate(password.rbegin(), password.rbegin() + steps, password.rend());
        } else if (fields[1] == "based") {
            char x = fields[6][0];
            int index = password.find(x);
            int steps = 1 + index;
            if (index >= 4) {
                steps++;
            }
            steps = steps % password.size();
            std::rotate(password.rbegin(), password.rbegin() + steps, password.rend());
        }
    } else if (fields[0] == "reverse") {
        int x = fields[2][0] - '0';
        int y = fields[4][0] - '0';
        std::reverse(password.begin() + x, password.begin() + y + 1);
    } else if (fields[0] == "move") {
        int x = fields[2][0] - '0';
        int y = fields[5][0] - '0';
        char c = password[x];
        password.erase(password.begin() + x);
        password.insert(password.begin() + y, c);
    }

    return password;
}

int main() {
    std::ifstream input_file("input.txt");
    if (!input_file.is_open()) {
        std::cout << "Error reading input file" << std::endl;
        return 1;
    }

    std::string input;
    std::string line;
    while (std::getline(input_file, line)) {
        input += line + "\n";
    }

    input_file.close();

    std::vector<std::string> operations;
    size_t start = 0;
    size_t end = input.find("\n");
    while (end != std::string::npos) {
        operations.push_back(input.substr(start, end - start));
        start = end + 1;
        end = input.find("\n", start);
    }

    std::string password = "abcdefgh";

    for (const std::string &op : operations) {
        password = applyOperation(op, password);
    }

    std::cout << password << std::endl;

    return 0;
}