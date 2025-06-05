
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <utility> // For std::pair
#include <algorithm> // For std::string::find_first_not_of, std::string::find_last_not_of

// Global keypad definitions (read-only)
const std::vector<std::string> KEY_PAD = {"789", "456", "123", " 0A"};
const std::vector<std::string> ROBOT_PAD = {" ^A", "<v>"};
const int MAX_ROBOTS = 3;

// Memoization table for the solve function
std::map<std::pair<std::string, int>, long long> memo;

// Helper function to find character position on a keypad
std::pair<int, int> find_position(const std::vector<std::string>& mat, char ch) {
    for (int i = 0; i < mat.size(); ++i) {
        for (int j = 0; j < mat[i].length(); ++j) {
            if (mat[i][j] == ch) {
                return {i, j};
            }
        }
    }
    return {-1, -1}; // Should not happen with valid problem input
}

// Helper function to check if a sequence of moves is valid on a keypad
bool ok(const std::vector<std::string>& mat, std::pair<int, int> st, const std::string& seq) {
    int curr_i = st.first;
    int curr_j = st.second;
    int max_rows = mat.size();
    if (max_rows == 0) return false;
    int max_cols = mat[0].length();

    // Check initial position validity
    if (curr_i < 0 || curr_i >= max_rows || curr_j < 0 || curr_j >= max_cols || mat[curr_i][curr_j] == ' ') {
        return false;
    }

    for (char ch_move : seq) {
        // Apply move
        if (ch_move == '^') {
            curr_i--;
        } else if (ch_move == 'v') {
            curr_i++;
        } else if (ch_move == '<') {
            curr_j--;
        } else if (ch_move == '>') {
            curr_j++;
        }

        // Check new position validity
        if (curr_i < 0 || curr_i >= max_rows || curr_j < 0 || curr_j >= max_cols || mat[curr_i][curr_j] == ' ') {
            return false;
        }
    }
    return true;
}

// Helper function to generate movement sequence
std::string generate_moves(std::pair<int, int> position, char objective,
                           const std::vector<std::string>& pad) {
    std::pair<int, int> obj_pos = find_position(pad, objective);
    int pos_i = position.first;
    int pos_j = position.second;
    int obj_i = obj_pos.first;
    int obj_j = obj_pos.second;

    std::string ret1 = "";
    if (pos_j > obj_j) ret1 += std::string(pos_j - obj_j, '<');
    if (pos_i > obj_i) ret1 += std::string(pos_i - obj_i, '^');
    if (pos_i < obj_i) ret1 += std::string(obj_i - pos_i, 'v');
    if (pos_j < obj_j) ret1 += std::string(obj_j - pos_j, '>');

    if (ok(pad, position, ret1)) {
        return ret1;
    }

    std::string ret2 = "";
    if (pos_j < obj_j) ret2 += std::string(obj_j - pos_j, '>');
    if (pos_i > obj_i) ret2 += std::string(pos_i - obj_i, '^');
    if (pos_i < obj_i) ret2 += std::string(obj_i - pos_i, 'v');
    if (pos_j > obj_j) ret2 += std::string(pos_j - obj_j, '<');

    if (ok(pad, position, ret2)) {
        return ret2;
    }
    return ""; // If neither sequence is valid
}

// Recursive function to solve the problem with memoization
long long solve(const std::string& code, int robots) {
    if (robots <= 0) {
        return code.length();
    }

    std::pair<std::string, int> state = {code, robots};
    if (memo.count(state)) {
        return memo[state];
    }

    long long ret = 0;
    std::pair<int, int> current_pos;
    const std::vector<std::string>* current_pad;

    // Determine initial position and keypad for the current robot based on whether it's the first
    if (robots == MAX_ROBOTS) {
        current_pos = {3, 2}; // 'A' on KEY_PAD
        current_pad = &KEY_PAD;
    } else {
        current_pos = {0, 2}; // 'A' on ROBOT_PAD
        current_pad = &ROBOT_PAD;
    }

    for (char ch : code) {
        std::string moves = generate_moves(current_pos, ch, *current_pad);
        
        // Update current_pos to the location of 'ch' on the current keypad
        current_pos = find_position(*current_pad, ch);
        
        ret += solve(moves + "A", robots - 1);
    }
    
    return memo[state] = ret;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        return 1; // Error opening file
    }

    long long total_result = 0;
    std::string line;

    while (std::getline(inputFile, line)) {
        // Trim leading and trailing whitespace
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
        // Extract digits from the entire line to form numeric_part
        for (char c : line) {
            if (c >= '0' && c <= '9') {
                numeric_part = numeric_part * 10 + (c - '0');
            }
        }
        
        // The entire stripped line is the 'code' for the solver
        std::string code = line; 

        memo.clear(); // Clear memoization table for each new input code
        long long sv = solve(code, MAX_ROBOTS);
        total_result += sv * numeric_part;
    }

    inputFile.close();
    std::cout << total_result << std::endl;

    return 0;
}

