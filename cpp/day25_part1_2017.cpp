
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <tuple>
#include <algorithm>
#include <sstream>

std::tuple<char, long long, std::map<char, std::map<int, std::tuple<int, int, char>>>> read_blueprint(const std::string& filename) {
    std::ifstream f(filename);
    std::string line;

    std::getline(f, line);
    char initial_state = line[line.length() - 2];

    std::getline(f, line);
    long long steps;
    size_t pos = line.find("steps");
    size_t start = line.rfind(' ', pos - 2) + 1;
    steps = std::stoll(line.substr(start, pos - start));

    std::map<char, std::map<int, std::tuple<int, int, char>>> states;
    char state_name;
    int val, write_val, move_dir;
    char next_state;

    while (std::getline(f, line)) {
        if (line.empty())
            continue;

        if (line.find("In state ") == 0) {
            state_name = line[line.length() - 2];
            for (int i = 0; i < 2; ++i) {
                std::getline(f, line); // If current value is X:
                val = line[line.length() - 2] - '0';

                std::getline(f, line); // Write the value Y.
                write_val = line[line.length() - 2] - '0';

                std::getline(f, line); // Move one slot to the <left/right>.
                move_dir = (line.find("left") != std::string::npos) ? -1 : 1;

                std::getline(f, line); // Continue with state Z.
                next_state = line[line.length() - 2];

                states[state_name][val] = std::make_tuple(write_val, move_dir, next_state);
            }
        }
    }

    return std::make_tuple(initial_state, steps, states);
}

long long run_turing_machine(char initial_state, long long steps, const std::map<char, std::map<int, std::tuple<int, int, char>>>& states) {
    std::map<long long, int> tape;
    long long cursor = 0;
    char state = initial_state;

    for (long long i = 0; i < steps; ++i) {
        int current_val = 0;
        if (tape.count(cursor)) {
            current_val = tape[cursor];
        }

        const auto& instructions = states.at(state).at(current_val);
        int write_val = std::get<0>(instructions);
        int move_dir = std::get<1>(instructions);
        char next_state = std::get<2>(instructions);

        tape[cursor] = write_val;
        cursor += move_dir;
        state = next_state;
    }

    long long checksum = 0;
    for (const auto& pair : tape) {
        checksum += pair.second;
    }
    return checksum;
}

int main() {
    auto blueprint = read_blueprint("input.txt");
    char initial_state = std::get<0>(blueprint);
    long long steps = std::get<1>(blueprint);
    const auto& states = std::get<2>(blueprint);

    long long checksum = run_turing_machine(initial_state, steps, states);
    std::cout << checksum << std::endl;

    return 0;
}
