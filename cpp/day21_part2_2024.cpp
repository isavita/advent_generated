
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <utility>
#include <fstream>
#include <algorithm>

class Solver {
public:
    std::vector<std::string> keyPad;
    std::vector<std::string> robotPad;
    int initialMaxRobots; 

    std::map<std::pair<std::string, int>, long long> memo;

    Solver(const std::vector<std::string>& kp, const std::vector<std::string>& rp, int imr)
        : keyPad(kp), robotPad(rp), initialMaxRobots(imr) {}

    std::pair<int, int> findPosition(const std::vector<std::string>& mat, char ch) {
        for (int i = 0; i < mat.size(); ++i) {
            for (int j = 0; j < mat[i].length(); ++j) {
                if (mat[i][j] == ch) {
                    return {i, j};
                }
            }
        }
        return {-1, -1}; 
    }

    bool ok(const std::vector<std::string>& mat, std::pair<int, int> st, const std::string& seq) {
        int curr_i = st.first;
        int curr_j = st.second;
        int mat_rows = mat.size();
        if (mat_rows == 0) return false;
        int mat_cols = mat[0].length();

        for (char ch : seq) {
            if (!(0 <= curr_i && curr_i < mat_rows && 0 <= curr_j && curr_j < mat_cols) || mat[curr_i][curr_j] == ' ') {
                return false;
            }
            if (ch == '^') {
                curr_i -= 1;
            } else if (ch == 'v') {
                curr_i += 1;
            } else if (ch == '<') {
                curr_j -= 1;
            } else if (ch == '>') {
                curr_j += 1;
            }
        }
        return true;
    }

    std::string generateMoves(std::pair<int, int> position, char objective, const std::vector<std::string>& pad) {
        std::pair<int, int> obj_pos = findPosition(pad, objective);
        int pos_i = position.first;
        int pos_j = position.second;
        int obj_pos_i = obj_pos.first;
        int obj_pos_j = obj_pos.second;

        std::string result_attempt1 = "";
        if (pos_j > obj_pos_j) {
            result_attempt1.append(pos_j - obj_pos_j, '<');
        }
        if (pos_i > obj_pos_i) {
            result_attempt1.append(pos_i - obj_pos_i, '^');
        }
        if (pos_i < obj_pos_i) {
            result_attempt1.append(obj_pos_i - pos_i, 'v');
        }
        if (pos_j < obj_pos_j) {
            result_attempt1.append(obj_pos_j - pos_j, '>');
        }

        if (ok(pad, position, result_attempt1)) {
            return result_attempt1;
        }

        std::string result_attempt2 = "";
        if (pos_j < obj_pos_j) {
            result_attempt2.append(obj_pos_j - pos_j, '>');
        }
        if (pos_i > obj_pos_i) {
            result_attempt2.append(pos_i - obj_pos_i, '^');
        }
        if (pos_i < obj_pos_i) {
            result_attempt2.append(obj_pos_i - pos_i, 'v');
        }
        if (pos_j > obj_pos_j) {
            result_attempt2.append(pos_j - obj_pos_j, '<');
        }
            
        return result_attempt2;
    }

    long long solve(const std::string& code, int robots) {
        std::pair<std::string, int> key = {code, robots};
        if (memo.count(key)) {
            return memo[key];
        }

        if (robots <= 0) {
            return code.length();
        }

        long long ret = 0;
        std::pair<int, int> current_robot_pos;

        if (robots == initialMaxRobots) {
            current_robot_pos = {3, 2};
        } else {
            current_robot_pos = {0, 2};
        }
        
        for (char ch : code) {
            std::string moves;
            const std::vector<std::string>* current_pad;

            if (robots == initialMaxRobots) {
                current_pad = &keyPad;
            } else {
                current_pad = &robotPad;
            }
            
            moves = generateMoves(current_robot_pos, ch, *current_pad);
            current_robot_pos = findPosition(*current_pad, ch);

            ret += solve(moves + "A", robots - 1);
        }

        memo[key] = ret;
        return ret;
    }
};

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    int max_robots = 26;
    std::vector<std::string> key_pad = {
        "789",
        "456",
        "123",
        " 0A",
    };
    std::vector<std::string> robot_pad = {
        " ^A",
        "<v>",
    };

    Solver solver(key_pad, robot_pad, max_robots);

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    long long total_ret = 0;
    std::string line;
    while (std::getline(inputFile, line)) {
        size_t first = line.find_first_not_of(" \t\n\r\f\v");
        if (std::string::npos == first) {
            line.clear();
        } else {
            size_t last = line.find_last_not_of(" \t\n\r\f\v");
            line = line.substr(first, (last - first + 1));
        }

        if (line.empty()) {
            continue;
        }

        long long numeric_part = 0;
        for (char char_in_line : line) {
            if (char_in_line >= '0' && char_in_line <= '9') {
                numeric_part = numeric_part * 10 + (char_in_line - '0');
            }
        }
        
        total_ret += solver.solve(line, max_robots) * numeric_part;
    }

    inputFile.close();

    std::cout << total_ret << std::endl;

    return 0;
}

