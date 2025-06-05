
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
#include <set>
#include <deque>
#include <tuple>
#include <fstream>
#include <utility>

std::pair<int, std::vector<int>> decode(long long n) {
    int op = n % 100;
    std::vector<int> modes(3);
    modes[0] = (n / 100) % 10;
    modes[1] = (n / 1000) % 10;
    modes[2] = (n / 10000) % 10;
    return {op, modes};
}

class Machine {
public:
    std::map<long long, long long> data;
    long long ip;
    std::deque<long long>* inStream;
    std::vector<long long>* outStream;
    long long relbase;

    Machine(const std::vector<long long>& program, std::deque<long long>* in, std::vector<long long>* out)
        : ip(0), inStream(in), outStream(out), relbase(0) {
        for (long long i = 0; i < program.size(); ++i) {
            data[i] = program[i];
        }
    }

    long long get(long long addr, int mode) {
        long long effective_addr;
        if (mode == 0) { // Position mode
            effective_addr = data[addr];
        } else if (mode == 1) { // Immediate mode
            effective_addr = addr;
        } else { // Relative mode (mode == 2)
            effective_addr = relbase + data[addr];
        }
        return data[effective_addr];
    }

    void set(long long addr, int mode, long long val) {
        long long effective_addr;
        if (mode == 0) { // Position mode
            effective_addr = data[addr];
        } else { // Relative mode (mode == 2)
            effective_addr = relbase + data[addr];
        }
        data[effective_addr] = val;
    }

    bool step() {
        std::pair<int, std::vector<int>> decoded_inst = decode(data[ip]);
        int op = decoded_inst.first;
        const std::vector<int>& modes = decoded_inst.second;

        long long val1, val2;

        switch (op) {
            case 1: // add
                val1 = get(ip + 1, modes[0]);
                val2 = get(ip + 2, modes[1]);
                set(ip + 3, modes[2], val1 + val2);
                ip += 4;
                break;
            case 2: // mul
                val1 = get(ip + 1, modes[0]);
                val2 = get(ip + 2, modes[1]);
                set(ip + 3, modes[2], val1 * val2);
                ip += 4;
                break;
            case 3: // input
                set(ip + 1, modes[0], inStream->front());
                inStream->pop_front();
                ip += 2;
                break;
            case 4: // output
                outStream->push_back(get(ip + 1, modes[0]));
                ip += 2;
                break;
            case 5: // jt (jump if true)
                val1 = get(ip + 1, modes[0]);
                val2 = get(ip + 2, modes[1]);
                if (val1 != 0) {
                    ip = val2;
                } else {
                    ip += 3;
                }
                break;
            case 6: // jf (jump if false)
                val1 = get(ip + 1, modes[0]);
                val2 = get(ip + 2, modes[1]);
                if (val1 == 0) {
                    ip = val2;
                } else {
                    ip += 3;
                }
                break;
            case 7: // lt (less than)
                val1 = get(ip + 1, modes[0]);
                val2 = get(ip + 2, modes[1]);
                set(ip + 3, modes[2], (val1 < val2) ? 1 : 0);
                ip += 4;
                break;
            case 8: // eq (equals)
                val1 = get(ip + 1, modes[0]);
                val2 = get(ip + 2, modes[1]);
                set(ip + 3, modes[2], (val1 == val2) ? 1 : 0);
                ip += 4;
                break;
            case 9: // rbo (relative base offset)
                relbase += get(ip + 1, modes[0]);
                ip += 2;
                break;
            case 99: // halt
                return false;
            default:
                return false;
        }
        return true;
    }

    void run() {
        while (step()) {
            // Continue stepping
        }
    }
};

std::vector<long long> run_machine(const std::vector<long long>& program, std::deque<long long>& in_stream_data) {
    std::vector<long long> out_stream_data;
    Machine machine(program, &in_stream_data, &out_stream_data);
    machine.run();
    return out_stream_data;
}

struct Point {
    int x, y;
    bool operator<(const Point& other) const {
        if (y != other.y) return y < other.y;
        return x < other.x;
    }
};

std::tuple<std::set<Point>, Point, int> parse(const std::vector<long long>& program) {
    std::deque<long long> in_stream_data;
    std::vector<long long> out = run_machine(program, in_stream_data);

    std::set<Point> scaffolding;
    Point robot = {0, 0};
    int dir = 0;

    int x = 0, y = 0;
    for (long long o : out) {
        char c = static_cast<char>(o);
        if (c == '\n') {
            y++;
            x = 0;
        } else {
            if (c == '^') {
                robot = {x, y};
                dir = 0;
                scaffolding.insert({x, y});
            } else if (c == '>') {
                robot = {x, y};
                dir = 1;
                scaffolding.insert({x, y});
            } else if (c == 'v') {
                robot = {x, y};
                dir = 2;
                scaffolding.insert({x, y});
            } else if (c == '<') {
                robot = {x, y};
                dir = 3;
                scaffolding.insert({x, y});
            } else if (c == '#') {
                scaffolding.insert({x, y});
            }
            x++;
        }
    }
    return std::make_tuple(scaffolding, robot, dir);
}

long long sum_align(const std::set<Point>& grid) {
    long long sum_ = 0;
    int dx[] = {0, 0, 1, -1};
    int dy[] = {1, -1, 0, 0};

    for (const auto& p : grid) {
        bool is_intersection = true;
        for (int i = 0; i < 4; ++i) {
            if (grid.find({p.x + dx[i], p.y + dy[i]}) == grid.end()) {
                is_intersection = false;
                break;
            }
        }
        if (is_intersection) {
            sum_ += static_cast<long long>(p.x) * p.y;
        }
    }
    return sum_;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    std::string line;
    std::getline(file, line);
    file.close();

    std::vector<long long> program;
    std::stringstream ss(line);
    std::string segment;

    while(std::getline(ss, segment, ',')) {
        program.push_back(std::stoll(segment));
    }

    std::set<Point> scaffolding;
    Point robot_pos;
    int robot_dir;

    std::tie(scaffolding, robot_pos, robot_dir) = parse(program);

    std::cout << sum_align(scaffolding) << std::endl;

    return 0;
}
