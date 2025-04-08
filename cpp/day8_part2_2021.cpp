
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <numeric>
#include <stdexcept> // Include for exceptions if needed, though omitted for brevity

// Helper function to sort characters in a string alphabetically
std::string alphabetize(std::string s) {
    std::sort(s.begin(), s.end());
    return s;
}

// Helper function to check if 'larger' string contains all characters of 'smaller' string
// Assumes both strings are already alphabetized
bool contains_all_chars(const std::string& larger, const std::string& smaller) {
    if (smaller.length() > larger.length()) {
        return false;
    }
    auto it_large = larger.begin();
    for (char c_small : smaller) {
        it_large = std::lower_bound(it_large, larger.end(), c_small);
        if (it_large == larger.end() || *it_large != c_small) {
            return false;
        }
        // Move past the found character in the larger string for the next search
         ++it_large;
    }
    return true;
}


int main() {
    std::ifstream infile("input.txt");
    if (!infile.is_open()) {
        std::cerr << "Error opening input.txt" << std::endl;
        return 1;
    }

    long long total_sum = 0;
    std::string line;

    while (std::getline(infile, line)) {
        std::stringstream ss(line);
        std::string segment;
        std::vector<std::string> all_patterns;
        all_patterns.reserve(14); // Pre-allocate space

        // Read 10 input patterns
        for (int i = 0; i < 10; ++i) {
            ss >> segment;
            all_patterns.push_back(alphabetize(segment));
        }

        // Skip the "|" separator
        std::string separator;
        ss >> separator;

        // Read 4 output patterns
        for (int i = 0; i < 4; ++i) {
            ss >> segment;
            all_patterns.push_back(alphabetize(segment));
        }

        if (all_patterns.size() != 14) {
             // Basic error check, though strict Python one is omitted
             // std::cerr << "Error: Invalid line format." << std::endl;
             continue;
        }

        std::vector<std::string> mapping(10);
        std::vector<std::string> five_segments; // To hold patterns with 5 segments (2, 3, 5)
        std::vector<std::string> six_segments;  // To hold patterns with 6 segments (0, 6, 9)

        // First pass: identify 1, 4, 7, 8 and collect 5/6 segment patterns
        for (int i = 0; i < 10; ++i) {
            const std::string& p = all_patterns[i];
            switch (p.length()) {
                case 2: mapping[1] = p; break;
                case 4: mapping[4] = p; break;
                case 3: mapping[7] = p; break;
                case 7: mapping[8] = p; break;
                case 5: five_segments.push_back(p); break; // 2, 3, 5
                case 6: six_segments.push_back(p); break;  // 0, 6, 9
            }
        }

        // Deduce 6-segment digits (0, 6, 9)
        for (const auto& p : six_segments) {
            if (!contains_all_chars(p, mapping[1])) {
                mapping[6] = p;
            } else if (contains_all_chars(p, mapping[4])) {
                mapping[9] = p;
            } else {
                mapping[0] = p;
            }
        }

         // Deduce 5-segment digits (2, 3, 5)
        for (const auto& p : five_segments) {
            if (contains_all_chars(p, mapping[1])) {
                mapping[3] = p;
            } else if (contains_all_chars(mapping[6], p)) {
                 mapping[5] = p;
            } else {
                mapping[2] = p;
            }
        }

        // Decode the output value
        int current_output_value = 0;
        for (int i = 10; i < 14; ++i) {
            const std::string& output_pattern = all_patterns[i];
            for (int digit = 0; digit < 10; ++digit) {
                if (output_pattern == mapping[digit]) {
                    current_output_value = current_output_value * 10 + digit;
                    break;
                }
            }
        }
        total_sum += current_output_value;
    }

    std::cout << total_sum << std::endl;

    return 0;
}
