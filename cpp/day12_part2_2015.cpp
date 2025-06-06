
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cctype>
#include <charconv>

// Forward declarations for the recursive parser
long long parse_value(const std::string& json, size_t& pos, bool ignore_red);

/**
 * @brief Skips whitespace characters in the input string.
 * @param json The input string.
 * @param pos The current position in the string (passed by reference).
 */
void skip_whitespace(const std::string& json, size_t& pos) {
    while (pos < json.length() && isspace(json[pos])) {
        pos++;
    }
}

/**
 * @brief Parses a number and returns its value. Advances the position past the number.
 * @param json The input string.
 * @param pos The current position in the string (passed by reference).
 * @return The parsed number as a long long.
 */
long long parse_number(const std::string& json, size_t& pos) {
    long long value = 0;
    auto [ptr, ec] = std::from_chars(json.data() + pos, json.data() + json.length(), value);
    if (ec == std::errc()) {
        pos = ptr - json.data();
    }
    return value;
}

/**
 * @brief Parses a string and returns true if its content is "red".
 * @param json The input string.
 * @param pos The current position in the string (passed by reference).
 * @return True if the string content is "red", false otherwise.
 */
bool parse_string(const std::string& json, size_t& pos) {
    pos++; // Skip opening '"'
    size_t start = pos;
    while (pos < json.length() && json[pos] != '"') {
        pos++;
    }
    // Check if the substring is "red"
    bool is_red = (pos - start == 3 && json.substr(start, 3) == "red");
    pos++; // Skip closing '"'
    return is_red;
}

/**
 * @brief Parses a JSON array and returns the sum of its numeric values.
 * @param json The input string.
 * @param pos The current position in the string (passed by reference).
 * @param ignore_red The flag for Part 2 logic.
 * @return The calculated sum.
 */
long long parse_array(const std::string& json, size_t& pos, bool ignore_red) {
    pos++; // Consume '['
    long long sum = 0;

    skip_whitespace(json, pos);
    if (pos < json.length() && json[pos] == ']') {
        pos++;
        return 0; // Empty array
    }

    while (pos < json.length()) {
        sum += parse_value(json, pos, ignore_red);
        skip_whitespace(json, pos);
        if (pos < json.length() && json[pos] == ']') {
            pos++;
            break;
        }
        pos++; // Consume ','
    }
    return sum;
}

/**
 * @brief Parses a JSON object and returns the sum of its numeric values.
 *        If ignore_red is true, it returns 0 if any property has the value "red".
 * @param json The input string.
 * @param pos The current position in the string (passed by reference).
 * @param ignore_red The flag for Part 2 logic.
 * @return The calculated sum.
 */
long long parse_object(const std::string& json, size_t& pos, bool ignore_red) {
    pos++; // Consume '{'
    long long sum = 0;
    bool has_red_value = false;

    skip_whitespace(json, pos);
    if (pos < json.length() && json[pos] == '}') {
        pos++;
        return 0; // Empty object
    }

    while (pos < json.length()) {
        // Key (always a string, its value doesn't contribute to the sum)
        parse_value(json, pos, ignore_red);

        // Colon
        skip_whitespace(json, pos);
        pos++; // Consume ':'

        // Peek at value to check for "red" before parsing it
        if (ignore_red && !has_red_value) {
            size_t peek_pos = pos;
            skip_whitespace(json, peek_pos);
            if (peek_pos < json.length() && json[peek_pos] == '"') {
                size_t temp_pos = peek_pos;
                if (parse_string(json, temp_pos)) {
                    has_red_value = true;
                }
            }
        }
        
        sum += parse_value(json, pos, ignore_red);
        
        skip_whitespace(json, pos);
        if (pos < json.length() && json[pos] == '}') {
            pos++;
            break;
        }
        pos++; // Consume ','
    }

    return (ignore_red && has_red_value) ? 0 : sum;
}

/**
 * @brief Main dispatcher for the parser. It determines the element type and calls the appropriate function.
 * @param json The input string.
 * @param pos The current position in the string (passed by reference).
 * @param ignore_red The flag for Part 2 logic.
 * @return The calculated sum for the parsed value.
 */
long long parse_value(const std::string& json, size_t& pos, bool ignore_red) {
    skip_whitespace(json, pos);
    if (pos >= json.length()) return 0;
    
    switch (json[pos]) {
        case '{':
            return parse_object(json, pos, ignore_red);
        case '[':
            return parse_array(json, pos, ignore_red);
        case '"':
            parse_string(json, pos);
            return 0; // Strings have a numeric value of 0
        default:
            // Assumes anything else is a number, per problem constraints
            return parse_number(json, pos);
    }
}

int main() {
    // Optimize C++ standard streams
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    // Read input from file
    std::ifstream file("input.txt");
    if (!file) {
        std::cerr << "Error: Could not open input.txt" << std::endl;
        return 1;
    }
    std::string json_data((std::istreambuf_iterator<char>(file)),
                           std::istreambuf_iterator<char>());

    // --- Part 1 ---
    size_t pos1 = 0;
    long long part1_sum = parse_value(json_data, pos1, false);
    std::cout << "Part 1: The sum of all numbers is " << part1_sum << ".\n";

    // --- Part 2 ---
    size_t pos2 = 0;
    long long part2_sum = parse_value(json_data, pos2, true);
    std::cout << "Part 2: The sum ignoring red objects is " << part2_sum << ".\n";

    return 0;
}
