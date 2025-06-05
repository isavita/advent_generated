
#include <iostream>
#include <vector>
#include <string>
#include <numeric>
#include <fstream>
#include <sstream>
#include <algorithm> // For std::fill

const int MAX_SPRINGS_LEN = 160;        // Max length of springs string after unfolding (e.g., 30*5 + 4)
const int MAX_GROUP_LEN = 60;           // Max length of group vector after unfolding (e.g., 10*5 + 1 for boundary)
const int MAX_CONTIGUOUS_DAMAGED = 25;  // Max value of a single group number + 1 (e.g., if max group value is 20)

long long memo[MAX_SPRINGS_LEN][MAX_GROUP_LEN][MAX_CONTIGUOUS_DAMAGED];
const long long UNCOMPUTED = -1;

struct Row {
    std::string springs;
    std::vector<int> group;
};

long long countArrangementsRecursive(const std::string& springs, const std::vector<int>& group,
                                     int i_springs, int i_group, int i_contiguous_damaged) {
    if (i_springs == springs.length()) {
        if (i_group == group.size() && i_contiguous_damaged == 0) {
            return 1;
        }
        if (i_group == group.size() - 1 && i_contiguous_damaged == group[i_group]) {
            return 1;
        }
        return 0;
    }

    if (memo[i_springs][i_group][i_contiguous_damaged] != UNCOMPUTED) {
        return memo[i_springs][i_group][i_contiguous_damaged];
    }

    long long res = 0;
    char current_char = springs[i_springs];

    // Case 1: Treat current_char as '.' (operational)
    if (current_char == '.' || current_char == '?') {
        if (i_contiguous_damaged == 0) {
            res += countArrangementsRecursive(springs, group, i_springs + 1, i_group, 0);
        } else if (i_group < group.size() && i_contiguous_damaged == group[i_group]) {
            res += countArrangementsRecursive(springs, group, i_springs + 1, i_group + 1, 0);
        }
    }

    // Case 2: Treat current_char as '#' (damaged)
    if (current_char == '#' || current_char == '?') {
        if (i_group < group.size() && i_contiguous_damaged < group[i_group]) {
            res += countArrangementsRecursive(springs, group, i_springs + 1, i_group, i_contiguous_damaged + 1);
        }
    }

    return memo[i_springs][i_group][i_contiguous_damaged] = res;
}

long long countArrangements(const Row& row) {
    for (int i = 0; i <= row.springs.length(); ++i) { // max index for i_springs is length()
        for (int j = 0; j <= row.group.size(); ++j) { // max index for i_group is size()
            std::fill(memo[i][j], memo[i][j] + MAX_CONTIGUOUS_DAMAGED, UNCOMPUTED);
        }
    }
    return countArrangementsRecursive(row.springs, row.group, 0, 0, 0);
}

Row unfoldRow(const Row& original_row, int unfolding_factor) {
    Row new_row;
    new_row.springs = original_row.springs;
    new_row.group = original_row.group;

    for (int i = 1; i < unfolding_factor; ++i) {
        new_row.springs += "?" + original_row.springs;
        new_row.group.insert(new_row.group.end(), original_row.group.begin(), original_row.group.end());
    }
    return new_row;
}

Row parseInputLine(const std::string& line) {
    Row row;
    size_t space_pos = line.find(' ');
    row.springs = line.substr(0, space_pos);

    std::string group_str = line.substr(space_pos + 1);
    std::stringstream ss(group_str);
    std::string segment;
    while (std::getline(ss, segment, ',')) {
        row.group.push_back(std::stoi(segment));
    }
    return row;
}

std::vector<std::string> readFile(const std::string& file_name) {
    std::vector<std::string> lines;
    std::ifstream file(file_name);
    if (!file.is_open()) {
        std::cerr << "Error opening file: " << file_name << std::endl;
        exit(1);
    }
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    file.close();
    return lines;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::vector<std::string> input_lines = readFile("input.txt");

    long long total_arrangements = 0;
    for (const std::string& line : input_lines) {
        Row original_row = parseInputLine(line);
        Row unfolded_row = unfoldRow(original_row, 5);
        total_arrangements += countArrangements(unfolded_row);
    }

    std::cout << total_arrangements << std::endl;

    return 0;
}
