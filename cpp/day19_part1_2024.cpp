
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <string_view>
#include <vector>
#include <algorithm> // Required for std::find_first_not_of, std::find_last_not_of

// Function to trim leading and trailing whitespace
std::string trim(const std::string& str) {
    const char* whitespace = " \t\n\r\f\v";
    size_t start = str.find_first_not_of(whitespace);
    if (start == std::string::npos) {
        return ""; // String contains only whitespace or is empty
    }
    size_t end = str.find_last_not_of(whitespace);
    return str.substr(start, end - start + 1);
}

bool can_make(const std::string& design, const std::vector<std::string>& patterns) {
    int n = design.length();
    if (n == 0) return true; // An empty design can always be made

    std::vector<bool> dp(n + 1, false);
    dp[0] = true;
    std::string_view design_view(design); // Use string_view for efficient substring comparison

    for (int i = 1; i <= n; ++i) {
        for (const std::string& p : patterns) {
            int lp = p.length();
            if (lp > i) continue; // Pattern is longer than the current prefix part

            // Check if the previous state was reachable and the current substring matches the pattern
            if (dp[i - lp] && design_view.substr(i - lp, lp) == p) {
                dp[i] = true;
                break; // Found a way to form the prefix of length i, move to the next i
            }
        }
    }
    return dp[n];
}

int main() {
    std::ios_base::sync_with_stdio(false); // Faster I/O
    // std::cin.tie(NULL); // Not needed as we use fstream

    std::ifstream fin("input.txt");
    if (!fin.is_open()) {
         // Cannot open file, maybe print error in real scenario
         return 1;
    }

    std::string line;
    std::vector<std::string> available_patterns;

    // Read the first line for patterns
    if (std::getline(fin, line)) {
        std::stringstream ss(line);
        std::string segment;
        while (std::getline(ss, segment, ',')) {
            std::string trimmed_segment = trim(segment);
            if (!trimmed_segment.empty()) {
                available_patterns.push_back(trimmed_segment);
            }
        }
    }

    // Read and discard the second line (assumed separator)
    std::getline(fin, line);

    int count = 0;
    std::string design;
    // Read subsequent lines for designs
    while (std::getline(fin, design)) {
        std::string trimmed_design = trim(design);
        if (!trimmed_design.empty()) { // Only process non-empty designs
            if (can_make(trimmed_design, available_patterns)) {
                count++;
            }
        }
    }

    fin.close(); // Close the file

    std::cout << count << std::endl;

    return 0;
}
