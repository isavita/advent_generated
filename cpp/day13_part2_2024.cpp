
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <numeric>
#include <utility>

struct Machine {
    long long ax, ay;
    long long bx, by;
    long long px, py;
};

long long parse_coordinate(const std::string& s) {
    size_t start = 0;
    while (start < s.length() && (s[start] == '+' || s[start] == '-' || !isdigit(s[start]))) {
        start++;
    }
    return std::stoll(s.substr(start));
}

std::pair<long long, long long> parse_point_line(const std::string& s) {
    size_t comma_pos = s.find(',');
    std::string x_part = s.substr(0, comma_pos);
    std::string y_part = s.substr(comma_pos + 1);
    return {parse_coordinate(x_part), parse_coordinate(y_part)};
}

Machine parse_machine(const std::vector<std::string>& lines) {
    Machine m = {0, 0, 0, 0, 0, 0};
    const std::string a_prefix = "Button A:";
    const std::string b_prefix = "Button B:";
    const std::string p_prefix = "Prize:";

    for (const std::string& line : lines) {
        if (line.rfind(a_prefix, 0) == 0) {
            auto p = parse_point_line(line.substr(a_prefix.length()));
            m.ax = p.first;
            m.ay = p.second;
        } else if (line.rfind(b_prefix, 0) == 0) {
            auto p = parse_point_line(line.substr(b_prefix.length()));
            m.bx = p.first;
            m.by = p.second;
        } else if (line.rfind(p_prefix, 0) == 0) {
            auto p = parse_point_line(line.substr(p_prefix.length()));
            m.px = p.first;
            m.py = p.second;
        }
    }
    return m;
}

std::vector<Machine> read_input(const std::string& filename) {
    std::vector<Machine> machines;
    std::ifstream file(filename);
    std::string line;
    std::vector<std::string> current_machine_lines;

    while (std::getline(file, line)) {
        size_t first = line.find_first_not_of(" \t\n\r\f\v");
        if (std::string::npos == first) {
            if (!current_machine_lines.empty()) {
                machines.push_back(parse_machine(current_machine_lines));
                current_machine_lines.clear();
            }
        } else {
            size_t last = line.find_last_not_of(" \t\n\r\f\v");
            current_machine_lines.push_back(line.substr(first, (last - first) + 1));
        }
    }
    if (!current_machine_lines.empty()) {
        machines.push_back(parse_machine(current_machine_lines));
    }
    return machines;
}

long long solve_machine(long long ax, long long ay, long long bx, long long by, long long px, long long py) {
    long long D = ax * by - ay * bx;
    if (D == 0) {
        return -1;
    }
    long long numA = px * by - py * bx;
    long long numB = -px * ay + py * ax;

    if (numA % D != 0 || numB % D != 0) {
        return -1;
    }
    long long a = numA / D;
    long long b = numB / D;

    if (a < 0 || b < 0) {
        return -1;
    }
    return 3 * a + b;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    const long long offset = 10000000000000LL;
    std::vector<Machine> machines = read_input("input.txt");
    std::vector<long long> results;

    for (const auto& m : machines) {
        long long cost = solve_machine(m.ax, m.ay, m.bx, m.by, m.px + offset, m.py + offset);
        if (cost >= 0) {
            results.push_back(cost);
        }
    }

    if (results.empty()) {
        std::cout << "0 0\n";
    } else {
        long long sum_results = std::accumulate(results.begin(), results.end(), 0LL);
        std::cout << results.size() << " " << sum_results << "\n";
    }

    return 0;
}
