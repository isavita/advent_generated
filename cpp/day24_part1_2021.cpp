
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <algorithm>

long long num(const std::vector<int>& w) {
    long long n = 0;
    for (int digit : w) {
        n *= 10;
        n += digit;
    }
    return n;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);

    std::vector<int> k, l, m;
    std::ifstream file("input.txt");
    std::string line;
    int line_idx = 0;

    while (std::getline(file, line)) {
        if (line_idx % 18 == 4) {
            size_t last_space = line.rfind(' ');
            int v = std::stoi(line.substr(last_space + 1));
            l.push_back(v);
        } else if (line_idx % 18 == 5) {
            size_t last_space = line.rfind(' ');
            int v = std::stoi(line.substr(last_space + 1));
            k.push_back(v);
        } else if (line_idx % 18 == 15) {
            size_t last_space = line.rfind(' ');
            int v = std::stoi(line.substr(last_space + 1));
            m.push_back(v);
        }
        line_idx++;
    }
    file.close();

    std::map<int, std::pair<int, int>> constraints;
    std::vector<int> stack;

    for (int i = 0; i < l.size(); ++i) {
        if (l[i] == 1) {
            stack.push_back(i);
        } else if (l[i] == 26) {
            int pop_idx = stack.back();
            stack.pop_back();
            constraints[pop_idx] = {i, m[pop_idx] + k[i]};
        }
    }

    std::vector<int> max_vals(14);

    for (int i = 0; i < 14; ++i) {
        if (constraints.count(i)) {
            int other_idx = constraints[i].first;
            int val = constraints[i].second;

            int vmax = 9;
            while (vmax + val > 9) {
                vmax--;
            }
            max_vals[i] = vmax;
            max_vals[other_idx] = vmax + val;
        }
    }

    std::cout << num(max_vals) << std::endl;

    return 0;
}
