
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <unordered_set>
#include <fstream>
#include <utility>

class BoardState {
public:
    std::vector<std::vector<int>> board;
    std::vector<std::vector<bool>> picked;
    int size;

    BoardState(const std::vector<std::vector<int>>& b) : board(b) {
        size = board.size();
        picked.resize(size, std::vector<bool>(size, false));
    }

    bool pick_num(int num) {
        int r_found = -1, c_found = -1;
        for (int r = 0; r < size; ++r) {
            for (int c = 0; c < size; ++c) {
                if (board[r][c] == num) {
                    picked[r][c] = true;
                    r_found = r;
                    c_found = c;
                    break;
                }
            }
            if (r_found != -1) break;
        }

        if (r_found == -1) return false;

        bool is_full_row = true;
        for (int c = 0; c < size; ++c) {
            if (!picked[r_found][c]) {
                is_full_row = false;
                break;
            }
        }
        if (is_full_row) return true;

        bool is_full_col = true;
        for (int r = 0; r < size; ++r) {
            if (!picked[r][c_found]) {
                is_full_col = false;
                break;
            }
        }
        return is_full_col;
    }

    int score() const {
        int s = 0;
        for (int r = 0; r < size; ++r) {
            for (int c = 0; c < size; ++c) {
                if (!picked[r][c]) {
                    s += board[r][c];
                }
            }
        }
        return s;
    }
};

std::pair<std::vector<int>, std::vector<BoardState>> parse_input(const std::string& input_data) {
    std::vector<int> nums;
    std::vector<BoardState> boards;

    std::vector<std::string> blocks;
    size_t current_pos = 0;
    size_t next_double_newline = input_data.find("\n\n");
    while (next_double_newline != std::string::npos) {
        blocks.push_back(input_data.substr(current_pos, next_double_newline - current_pos));
        current_pos = next_double_newline + 2;
        next_double_newline = input_data.find("\n\n", current_pos);
    }
    blocks.push_back(input_data.substr(current_pos));

    if (!blocks.empty()) {
        std::stringstream num_ss(blocks[0]);
        std::string num_str;
        while (std::getline(num_ss, num_str, ',')) {
            nums.push_back(std::stoi(num_str));
        }
    }

    for (size_t i = 1; i < blocks.size(); ++i) {
        std::vector<std::vector<int>> current_board_data;
        std::stringstream board_block_ss(blocks[i]);
        std::string board_line;
        while (std::getline(board_block_ss, board_line)) {
            if (board_line.empty()) continue;

            std::vector<int> row;
            std::stringstream row_ss(board_line);
            int val;
            while (row_ss >> val) {
                row.push_back(val);
            }
            if (!row.empty()) {
                current_board_data.push_back(row);
            }
        }
        if (!current_board_data.empty()) {
            boards.push_back(BoardState(current_board_data));
        }
    }

    return {nums, boards};
}

int solve(const std::string& input_data) {
    std::pair<std::vector<int>, std::vector<BoardState>> parsed_data = parse_input(input_data);
    std::vector<int> nums = parsed_data.first;
    std::vector<BoardState> boards = parsed_data.second;

    int last_winning_score = -1;
    std::unordered_set<int> already_won_board_indices;

    for (int n : nums) {
        for (int bi = 0; bi < boards.size(); ++bi) {
            if (already_won_board_indices.count(bi)) {
                continue;
            }

            bool did_win = boards[bi].pick_num(n);

            if (did_win) {
                last_winning_score = boards[bi].score() * n;
                already_won_board_indices.insert(bi);
            }
        }
    }
    return last_winning_score;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error: Could not open input.txt" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string input_data = buffer.str();
    file.close();

    int result = solve(input_data);

    std::cout << result << std::endl;

    return 0;
}
