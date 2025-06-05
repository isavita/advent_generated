
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
#include <set>
#include <algorithm>
#include <fstream>
#include <tuple>

class IntcodeComputer {
public:
    long long ip;
    long long relativeBase;
    std::map<long long, long long> memory;
    bool halted;

    bool awaitingInput;
    long long inputTargetAddr;
    int inputTargetMode;

    IntcodeComputer(const std::vector<long long>& program) : ip(0), relativeBase(0), halted(false), awaitingInput(false) {
        for (long long i = 0; i < program.size(); ++i) {
            memory[i] = program[i];
        }
    }

    long long getParameter(int mode, long long offset) {
        long long param = memory[ip + offset];
        if (mode == 0) {
            return memory[param];
        } else if (mode == 1) {
            return param;
        } else if (mode == 2) {
            return memory[relativeBase + param];
        }
        return 0;
    }

    void setParameter(int mode, long long offset, long long value) {
        long long param = memory[ip + offset];
        if (mode == 0) {
            memory[param] = value;
        } else if (mode == 2) {
            memory[relativeBase + param] = value;
        }
    }

    std::tuple<bool, bool, long long> runUntilIO(long long input_val) {
        if (halted) {
            return {false, true, 0};
        }

        if (awaitingInput) {
            setParameter(inputTargetMode, inputTargetAddr - ip, input_val);
            awaitingInput = false;
            ip += 2;
        }

        while (true) {
            long long instruction = memory[ip];
            int opcode = instruction % 100;
            int mode1 = (instruction / 100) % 10;
            int mode2 = (instruction / 1000) % 10;
            int mode3 = (instruction / 10000) % 10;

            if (opcode == 1) {
                long long p1 = getParameter(mode1, 1);
                long long p2 = getParameter(mode2, 2);
                setParameter(mode3, 3, p1 + p2);
                ip += 4;
            } else if (opcode == 2) {
                long long p1 = getParameter(mode1, 1);
                long long p2 = getParameter(mode2, 2);
                setParameter(mode3, 3, p1 * p2);
                ip += 4;
            } else if (opcode == 3) {
                inputTargetAddr = ip + 1;
                inputTargetMode = mode1;
                awaitingInput = true;
                return {false, false, 0};
            } else if (opcode == 4) {
                long long output_val = getParameter(mode1, 1);
                ip += 2;
                return {true, false, output_val};
            } else if (opcode == 5) {
                long long p1 = getParameter(mode1, 1);
                long long p2 = getParameter(mode2, 2);
                if (p1 != 0) {
                    ip = p2;
                } else {
                    ip += 3;
                }
            } else if (opcode == 6) {
                long long p1 = getParameter(mode1, 1);
                long long p2 = getParameter(mode2, 2);
                if (p1 == 0) {
                    ip = p2;
                } else {
                    ip += 3;
                }
            } else if (opcode == 7) {
                long long p1 = getParameter(mode1, 1);
                long long p2 = getParameter(mode2, 2);
                setParameter(mode3, 3, (p1 < p2) ? 1 : 0);
                ip += 4;
            } else if (opcode == 8) {
                long long p1 = getParameter(mode1, 1);
                long long p2 = getParameter(mode2, 2);
                setParameter(mode3, 3, (p1 == p2) ? 1 : 0);
                ip += 4;
            } else if (opcode == 9) {
                long long p1 = getParameter(mode1, 1);
                relativeBase += p1;
                ip += 2;
            } else if (opcode == 99) {
                halted = true;
                return {false, true, 0};
            }
        }
    }
};

struct Point {
    int x, y;
    bool operator<(const Point& other) const {
        if (x != other.x) return x < other.x;
        return y < other.y;
    }
};

class Robot {
public:
    IntcodeComputer computer;
    int direction;
    Point position;
    std::map<Point, int> panels;
    std::set<Point> paintedPanels;

    Robot(const std::vector<long long>& program, int startPanelColor) : computer(program), direction(0), position({0, 0}) {
        panels[position] = startPanelColor;
    }

    void turnAndMove(int turnDirection) {
        if (turnDirection == 0) {
            direction = (direction - 1 + 4) % 4;
        } else if (turnDirection == 1) {
            direction = (direction + 1) % 4;
        }

        if (direction == 0) {
            position.y--;
        } else if (direction == 1) {
            position.x++;
        } else if (direction == 2) {
            position.y++;
        } else if (direction == 3) {
            position.x--;
        }
    }

    void run() {
        bool output_pending = false;
        long long paint_color = 0;

        while (!computer.halted) {
            long long current_input = panels[position];
            auto [is_output, is_halted, output_val] = computer.runUntilIO(current_input);

            if (is_halted) {
                break;
            }

            if (is_output) {
                if (!output_pending) {
                    paint_color = output_val;
                    output_pending = true;
                } else {
                    int turn_direction = static_cast<int>(output_val);
                    output_pending = false;

                    panels[position] = static_cast<int>(paint_color);
                    paintedPanels.insert(position);
                    turnAndMove(turn_direction);
                }
            }
        }
    }

    int getPaintedPanelsCount() const {
        return paintedPanels.size();
    }

    void renderPanels() const {
        if (panels.empty()) {
            std::cout << "No panels painted.\n";
            return;
        }

        int min_x = 0, max_x = 0, min_y = 0, max_y = 0;
        bool first = true;
        for (const auto& pair : panels) {
            const Point& p = pair.first;
            if (first) {
                min_x = max_x = p.x;
                min_y = max_y = p.y;
                first = false;
            } else {
                min_x = std::min(min_x, p.x);
                max_x = std::max(max_x, p.x);
                min_y = std::min(min_y, p.y);
                max_y = std::max(max_y, p.y);
            }
        }

        std::cout << "\nRegistration Identifier:\n";
        for (int y = min_y; y <= max_y; ++y) {
            for (int x = min_x; x <= max_x; ++x) {
                Point current_p = {x, y};
                auto it = panels.find(current_p);
                if (it != panels.end() && it->second == 1) {
                    std::cout << '#';
                } else {
                    std::cout << ' ';
                }
            }
            std::cout << '\n';
        }
    }
};

std::vector<long long> parseInput(const std::string& filePath) {
    std::vector<long long> program;
    std::ifstream file(filePath);
    std::string line;
    if (file.is_open()) {
        std::getline(file, line);
        std::stringstream ss(line);
        std::string segment;
        while (std::getline(ss, segment, ',')) {
            program.push_back(std::stoll(segment));
        }
        file.close();
    }
    return program;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::string input_file = "input.txt";
    std::vector<long long> program = parseInput(input_file);

    Robot robotPart1(program, 0);
    robotPart1.run();
    int painted_count_part1 = robotPart1.getPaintedPanelsCount();
    std::cout << painted_count_part1 << '\n';

    Robot robotPart2(program, 1);
    robotPart2.run();
    robotPart2.renderPanels();

    return 0;
}

