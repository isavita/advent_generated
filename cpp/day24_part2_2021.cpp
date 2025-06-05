
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <stack>
#include <unordered_map>
#include <utility>
#include <algorithm>

int get_nth_token_as_int(const std::string& line, int n) {
    std::stringstream ss(line);
    std::string token;
    for (int i = 0; i <= n; ++i) {
        ss >> token;
    }
    return std::stoi(token);
}

long long num(const std::vector<int>& w) {
    long long n = 0;
    for (int digit : w) {
        n = n * 10 + digit;
    }
    return n;
}

int main() {
    std::vector<int> k, l, m;
    std::ifstream file("input.txt");
    std::string line;
    int i = 0;

    while (std::getline(file, line)) {
        if (i % 18 == 4) {
            l.push_back(get_nth_token_as_int(line, 2));
        } else if (i % 18 == 5) {
            k.push_back(get_nth_token_as_int(line, 2));
        } else if (i % 18 == 15) {
            m.push_back(get_nth_token_as_int(line, 2));
        }
        i++;
    }
    file.close();

    std::unordered_map<int, std::pair<int, int>> constraints;
    std::stack<int> s;

    for (size_t j = 0; j < l.size(); ++j) {
        if (l[j] == 1) {
            s.push(j);
        } else if (l[j] == 26) {
            int pop_idx = s.top();
            s.pop();
            constraints[pop_idx] = {static_cast<int>(j), m[pop_idx] + k[j]};
        }
    }

    std::vector<int> min_val(14, 0);

    for (int idx = 0; idx < 14; ++idx) {
        auto it = constraints.find(idx);
        if (it == constraints.end()) {
            continue;
        }

        int C = it->second.second;
        
        int digit_i = 1;
        while (digit_i + C < 1) {
            digit_i++;
        }

        min_val[idx] = digit_i;
        min_val[it->second.first] = digit_i + C;
    }

    std::cout << num(min_val) << std::endl;

    return 0;
}
