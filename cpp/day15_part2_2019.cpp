
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <deque>
#include <set>
#include <queue>
#include <sstream>
#include <algorithm>
#include <cmath>
#include <fstream>
#include <limits>
#include <tuple>

class Machine {
public:
    std::map<long long, long long> data;
    long long ip;
    std::deque<long long>& input_queue;
    std::deque<long long>& output_queue;
    long long relbase;

    Machine(const std::vector<long long>& program, std::deque<long long>& input_q, std::deque<long long>& output_q)
        : ip(0), input_queue(input_q), output_queue(output_q), relbase(0) {
        for (size_t i = 0; i < program.size(); ++i) {
            data[i] = program[i];
        }
    }

    long long get_val(long long addr, int mode) {
        if (mode == 0) return data[data[addr]];
        if (mode == 1) return data[addr];
        if (mode == 2) return data[relbase + data[addr]];
        return -1;
    }

    void set_val(long long addr, int mode, long long val) {
        if (mode == 0) data[data[addr]] = val;
        else if (mode == 2) data[relbase + data[addr]] = val;
    }

    bool step() {
        long long opcode_full = data[ip];
        int op = opcode_full % 100;
        int mode1 = (opcode_full / 100) % 10;
        int mode2 = (opcode_full / 1000) % 10;
        int mode3 = (opcode_full / 10000) % 10;

        if (op == 1) {
            set_val(ip + 3, mode3, get_val(ip + 1, mode1) + get_val(ip + 2, mode2));
            ip += 4;
        } else if (op == 2) {
            set_val(ip + 3, mode3, get_val(ip + 1, mode1) * get_val(ip + 2, mode2));
            ip += 4;
        } else if (op == 3) {
            if (input_queue.empty()) return false;
            set_val(ip + 1, mode1, input_queue.front());
            input_queue.pop_front();
            ip += 2;
        } else if (op == 4) {
            output_queue.push_back(get_val(ip + 1, mode1));
            ip += 2;
        } else if (op == 5) {
            ip = (get_val(ip + 1, mode1) != 0) ? get_val(ip + 2, mode2) : ip + 3;
        } else if (op == 6) {
            ip = (get_val(ip + 1, mode1) == 0) ? get_val(ip + 2, mode2) : ip + 3;
        } else if (op == 7) {
            set_val(ip + 3, mode3, (get_val(ip + 1, mode1) < get_val(ip + 2, mode2)) ? 1 : 0);
            ip += 4;
        } else if (op == 8) {
            set_val(ip + 3, mode3, (get_val(ip + 1, mode1) == get_val(ip + 2, mode2)) ? 1 : 0);
            ip += 4;
        } else if (op == 9) {
            relbase += get_val(ip + 1, mode1);
            ip += 2;
        } else if (op == 99) {
            return false;
        } else {
            return false;
        }
        return true;
    }

    void run() {
        while (step());
    }
};

int manhattan(std::pair<int, int> p, std::pair<int, int> q) {
    return std::abs(p.first - q.first) + std::abs(p.second - q.second);
}

class Pathfinder {
public:
    std::deque<long long> input_q;
    std::deque<long long> output_q;
    Machine machine;
    std::map<std::pair<int, int>, char> grid;
    std::map<int, std::pair<int, int>> dirmap;
    std::pair<int, int> p;
    std::pair<int, int> oxygen_pos;

    Pathfinder(const std::vector<long long>& program)
        : input_q(), output_q(), machine(program, input_q, output_q), p({0, 0}), oxygen_pos({-1,-1}) {
        grid[{0, 0}] = '.';
        dirmap[1] = {0, 1};
        dirmap[2] = {0, -1};
        dirmap[3] = {-1, 0};
        dirmap[4] = {1, 0};
    }

    bool try_move(int dir) {
        input_q.push_back(dir);
        machine.run();

        if (output_q.empty()) return false;
        long long output = output_q.front();
        output_q.pop_front();

        std::pair<int, int> next_pos = {p.first + dirmap[dir].first, p.second + dirmap[dir].second};

        if (output == 0) grid[next_pos] = '#';
        else if (output == 1) grid[next_pos] = '.';
        else if (output == 2) {
            grid[next_pos] = 'O';
            oxygen_pos = next_pos;
        }

        if (output != 0) p = next_pos;
        return output != 0;
    }

    std::set<std::pair<int, int>> get_open_cells() {
        std::set<std::pair<int, int>> open_cells;
        for (const auto& entry : grid) {
            std::pair<int, int> pos = entry.first;
            char val = entry.second;

            if (val == '#') continue;

            bool has_unexplored_neighbor = false;
            for (const auto& pair_dir_delta : dirmap) {
                std::pair<int, int> delta = pair_dir_delta.second;
                std::pair<int, int> neighbor_pos = {pos.first + delta.first, pos.second + delta.second};
                if (grid.find(neighbor_pos) == grid.end()) {
                    has_unexplored_neighbor = true;
                    break;
                }
            }
            if (has_unexplored_neighbor) open_cells.insert(pos);
        }
        return open_cells;
    }

    std::vector<int> shortest_path(std::pair<int, int> start, std::pair<int, int> end) {
        using State = std::tuple<int, std::pair<int, int>, std::vector<int>>;
        std::priority_queue<State, std::vector<State>, std::greater<State>> pq;
        std::map<std::pair<int, int>, int> distances;

        pq.push({0, start, {}});
        distances[start] = 0;

        while (!pq.empty()) {
            auto [dist, pos, path] = pq.top();
            pq.pop();

            if (pos == end) return path;
            if (dist > distances[pos]) continue;

            for (const auto& pair_dir_delta : dirmap) {
                int dir = pair_dir_delta.first;
                std::pair<int, int> delta = pair_dir_delta.second;
                std::pair<int, int> next_pos = {pos.first + delta.first, pos.second + delta.second};

                if (grid.count(next_pos) && grid[next_pos] != '#') {
                    int new_dist = dist + 1;
                    if (distances.find(next_pos) == distances.end() || new_dist < distances[next_pos]) {
                        distances[next_pos] = new_dist;
                        std::vector<int> new_path = path;
                        new_path.push_back(dir);
                        pq.push({new_dist, next_pos, new_path});
                    }
                }
            }
        }
        throw std::runtime_error("No path found");
    }

    void explore() {
        while (true) {
            std::set<std::pair<int, int>> open_cells = get_open_cells();
            if (open_cells.empty()) break;

            if (open_cells.find(p) == open_cells.end()) {
                int min_dist = std::numeric_limits<int>::max();
                std::pair<int, int> next_target_pos;

                for (const auto& pos_in_open : open_cells) {
                    int dist = manhattan(p, pos_in_open);
                    if (dist < min_dist) {
                        min_dist = dist;
                        next_target_pos = pos_in_open;
                    }
                }
                
                std::vector<int> path_to_target = shortest_path(p, next_target_pos);
                for (int move_dir : path_to_target) {
                    if (!try_move(move_dir)) throw std::runtime_error("Bad path during exploration");
                }
            }

            while (true) {
                int dir_to_try = -1;
                for (const auto& pair_dir_delta : dirmap) {
                    int current_dir = pair_dir_delta.first;
                    std::pair<int, int> delta = pair_dir_delta.second;
                    std::pair<int, int> next_pos = {p.first + delta.first, p.second + delta.second};

                    if (grid.find(next_pos) == grid.end()) {
                        dir_to_try = current_dir;
                        break;
                    }
                }

                if (dir_to_try == -1) break;
                if (!try_move(dir_to_try)) break;
            }
        }
    }

    int longest_path(std::pair<int, int> start) {
        using State = std::pair<int, std::pair<int, int>>;
        std::priority_queue<State, std::vector<State>, std::greater<State>> pq;
        std::map<std::pair<int, int>, int> distances;

        pq.push({0, start});
        distances[start] = 0;

        int max_dist = 0;

        while (!pq.empty()) {
            auto [dist, pos] = pq.top();
            pq.pop();

            if (dist > distances[pos]) continue;
            max_dist = std::max(max_dist, dist);

            for (const auto& pair_dir_delta : dirmap) {
                std::pair<int, int> delta = pair_dir_delta.second;
                std::pair<int, int> next_pos = {pos.first + delta.first, pos.second + delta.second};

                if (grid.count(next_pos) && grid[next_pos] != '#') {
                    int new_dist = dist + 1;
                    if (distances.find(next_pos) == distances.end() || new_dist < distances[next_pos]) {
                        distances[next_pos] = new_dist;
                        pq.push({new_dist, next_pos});
                    }
                }
            }
        }
        return max_dist;
    }
};

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<long long> program;
    std::string line;
    std::ifstream file("input.txt");

    std::getline(file, line);
    file.close();

    std::stringstream ss(line);
    std::string segment;
    while (std::getline(ss, segment, ',')) {
        program.push_back(std::stoll(segment));
    }

    Pathfinder pathfinder(program);
    pathfinder.explore();
    std::cout << pathfinder.longest_path(pathfinder.oxygen_pos) << std::endl;

    return 0;
}
