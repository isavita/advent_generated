
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <numeric> // Not strictly needed but good practice if summing later

// Function to evaluate expression according to the specific rules (left-to-right)
long long evaluate(std::string expression) {
    // Recursively evaluate expressions within parentheses first
    while (true) {
        size_t start = expression.rfind('(');
        if (start == std::string::npos) {
            break; // No more opening parentheses
        }
        // Find the matching ')' for the rightmost '('
        size_t end = expression.find(')', start + 1);
        // Extract the sub-expression inside the parentheses
        std::string sub_expr = expression.substr(start + 1, end - start - 1);
        // Evaluate the sub-expression recursively
        long long sub_result = evaluate(sub_expr);
        // Replace the parenthesis and its content with the result
        expression.replace(start, end - start + 1, std::to_string(sub_result));
    }

    // Evaluate the flattened expression (no parentheses) left-to-right
    std::stringstream ss(expression);
    long long total;
    long long operand;
    char op;

    // Read the first number
    ss >> total;

    // Read operator and subsequent numbers
    while (ss >> op >> operand) {
        if (op == '+') {
            total += operand;
        } else if (op == '*') {
            total *= operand;
        }
    }
    return total;
}

int main() {
    std::ifstream fin("input.txt");
    std::string line;
    long long total_sum = 0;

    // Read each line from the file
    while (std::getline(fin, line)) {
        if (!line.empty()) { // Process non-empty lines
            total_sum += evaluate(line);
        }
    }

    // Print the final sum
    std::cout << total_sum << std::endl;

    return 0;
}
