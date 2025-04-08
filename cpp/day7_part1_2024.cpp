
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <sstream>
#include <numeric>

// Function to evaluate if a target value can be achieved with the numbers
bool can_evaluate(long long target, const std::vector<long long>& numbers) {
    if (numbers.empty()) {
        return false;
    }
    if (numbers.size() == 1) {
        return numbers[0] == target;
    }

    size_t num_ops = numbers.size() - 1;
    size_t total_combinations = 1ULL << num_ops; // Use unsigned long long for bit shift clarity

    for (size_t i = 0; i < total_combinations; ++i) {
        long long current_result = numbers[0];
        size_t temp_i = i; // Use a copy for bit checking

        for (size_t j = 0; j < num_ops; ++j) {
            // Check the j-th bit (from right to left)
            bool use_multiply = (temp_i & 1);
            temp_i >>= 1;

            if (use_multiply) { // 1 -> '*'
                current_result *= numbers[j + 1];
            } else { // 0 -> '+'
                current_result += numbers[j + 1];
            }
        }

        if (current_result == target) {
            return true; // Found a valid combination
        }
    }
    return false; // No combination worked
}

int main() {
    // Optimize C++ standard streams
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);
    std::cout.tie(NULL);


    std::ifstream file("input.txt");
    if (!file.is_open()) {
        // Minimal error handling as per "no explanation" rule, but good practice
        return 1;
    }

    long long total_sum = 0;
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        size_t colon_pos = line.find(": ");
        if (colon_pos == std::string::npos) continue; // Skip malformed lines

        try {
            long long target = std::stoll(line.substr(0, colon_pos));

            std::vector<long long> numbers;
            std::stringstream ss(line.substr(colon_pos + 2));
            long long num;
            while (ss >> num) {
                numbers.push_back(num);
            }

            if (!numbers.empty()) {
                if (can_evaluate(target, numbers)) {
                    total_sum += target;
                }
            }
        } catch (const std::exception& e) {
            // Catch potential errors from stoll or string operations on bad input
            continue; 
        }
    }

    std::cout << total_sum << std::endl;

    return 0;
}
