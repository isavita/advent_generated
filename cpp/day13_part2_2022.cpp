
#include <iostream>     // For std::cout, std::cerr, std::endl
#include <fstream>      // For std::ifstream
#include <vector>       // For std::vector
#include <string>       // For std::string, std::getline, std::to_string
#include <variant>      // For std::variant (C++17)
#include <algorithm>    // For std::sort
#include <cctype>       // For isdigit

// Forward declaration needed for recursive type definition
struct Packet;

// PacketValue can be an int or a Packet (which is a list of PacketValues)
// Using std::variant enables this recursive structure
using PacketValue = std::variant<int, Packet>;

// A Packet is essentially a list of PacketValues
struct Packet {
    std::vector<PacketValue> elements;

    // Helper for debugging: converts Packet to a string representation
    std::string to_string() const {
        std::string s = "[";
        for (size_t i = 0; i < elements.size(); ++i) {
            if (i > 0) s += ",";
            if (std::holds_alternative<int>(elements[i])) {
                s += std::to_string(std::get<int>(elements[i]));
            } else {
                s += std::get<Packet>(elements[i]).to_string();
            }
        }
        s += "]";
        return s;
    }
};

// Enum to represent the three possible comparison outcomes
enum CompareResult {
    IN_ORDER = -1,      // Left is smaller than Right
    EQUAL = 0,          // Left is equal to Right
    NOT_IN_ORDER = 1    // Left is larger than Right
};

// Recursive parsing function to convert a substring into a PacketValue
// s: The input string being parsed
// pos: Current position in the string (passed by reference to update across recursive calls)
PacketValue parse_value(const std::string& s, int& pos) {
    if (s[pos] == '[') {
        pos++; // Consume '['
        Packet current_packet_list; // This will hold the elements of the current list
        while (s[pos] != ']') {
            current_packet_list.elements.push_back(parse_value(s, pos)); // Recursively parse child value
            if (s[pos] == ',') {
                pos++; // Consume ',' if present
            }
        }
        pos++; // Consume ']'
        return current_packet_list; // Return the completed list as a PacketValue
    } else {
        // It must be an integer
        int val = 0;
        // Parse multi-digit numbers
        while (pos < s.length() && isdigit(s[pos])) {
            val = val * 10 + (s[pos] - '0');
            pos++;
        }
        return val; // Return the integer as a PacketValue
    }
}

// Top-level function to parse an entire string representation of a packet into a Packet object
Packet parse_string_to_packet(const std::string& s) {
    int pos = 0;
    // The problem states that each packet is always a list, so the top-level parse_value call
    // will always return a PacketValue holding a Packet.
    return std::get<Packet>(parse_value(s, pos));
}

// Recursive comparison function for two PacketValues
// This function implements the core comparison rules defined in the problem
CompareResult compare_values(const PacketValue& left_val, const PacketValue& right_val) {
    // Rule 1: Both values are integers
    if (std::holds_alternative<int>(left_val) && std::holds_alternative<int>(right_val)) {
        int l = std::get<int>(left_val);
        int r = std::get<int>(right_val);
        if (l < r) return IN_ORDER;
        if (l > r) return NOT_IN_ORDER;
        return EQUAL; // Integers are equal
    }
    // Rule 2: Both values are lists
    if (std::holds_alternative<Packet>(left_val) && std::holds_alternative<Packet>(right_val)) {
        const Packet& l_list = std::get<Packet>(left_val);
        const Packet& r_list = std::get<Packet>(right_val);

        size_t i = 0;
        // Compare elements one by one until a decision is made or one list runs out
        while (i < l_list.elements.size() && i < r_list.elements.size()) {
            CompareResult res = compare_values(l_list.elements[i], r_list.elements[i]);
            if (res != EQUAL) {
                return res; // Decision made, return it
            }
            i++;
        }
        // If we reached here, all compared elements were equal. Now check list lengths.
        if (i == l_list.elements.size() && i < r_list.elements.size()) return IN_ORDER;     // Left list ran out first
        if (i < l_list.elements.size() && i == r_list.elements.size()) return NOT_IN_ORDER; // Right list ran out first
        return EQUAL; // Both lists ran out at the same time and were equal
    }

    // Rule 3: Mixed types (one integer, one list)
    // Convert the integer to a list containing only that integer, then retry comparison.
    
    // Left is int, Right is list
    if (std::holds_alternative<int>(left_val) && std::holds_alternative<Packet>(right_val)) {
        Packet temp_l_list;
        // Implicit conversion from int to PacketValue, then push_back
        temp_l_list.elements.push_back(left_val); 
        return compare_values(temp_l_list, right_val); // Retry comparison with converted left side
    }
    // Left is list, Right is int
    if (std::holds_alternative<Packet>(left_val) && std::holds_alternative<int>(right_val)) {
        Packet temp_r_list;
        // Implicit conversion from int to PacketValue, then push_back
        temp_r_list.elements.push_back(right_val); 
        return compare_values(left_val, temp_r_list); // Retry comparison with converted right side
    }
    
    // This part should ideally not be reached if inputs are always valid PacketValues
    // and all type combinations are covered by the rules.
    return EQUAL; 
}

// Top-level comparison function for two Packet objects
// Since a Packet is always a list, we can treat them as PacketValue objects of type Packet
// and use the general compare_values function.
CompareResult compare_packets(const Packet& left, const Packet& right) {
    return compare_values(PacketValue(left), PacketValue(right));
}


int main() {
    // Open the input file
    std::ifstream inputFile("input.txt");
    if (!inputFile.is_open()) {
        std::cerr << "Error: Could not open input.txt" << std::endl;
        return 1;
    }

    std::string line;
    std::vector<std::string> packet_strings;
    // Read all non-empty lines (packet strings) from the input file
    while (std::getline(inputFile, line)) {
        if (!line.empty()) {
            packet_strings.push_back(line);
        }
    }
    inputFile.close();

    // --- Part 1 ---
    long long sum_of_correct_indices = 0;
    int pair_index = 1; // Puzzle uses 1-based indexing for pairs

    // Process packets in pairs
    for (size_t i = 0; i < packet_strings.size(); i += 2) {
        Packet left_packet = parse_string_to_packet(packet_strings[i]);
        Packet right_packet = parse_string_to_packet(packet_strings[i+1]);

        if (compare_packets(left_packet, right_packet) == IN_ORDER) {
            sum_of_correct_indices += pair_index;
        }
        pair_index++;
    }
    std::cout << "Part 1: Sum of indices of correct pairs: " << sum_of_correct_indices << std::endl;

    // --- Part 2 ---
    std::vector<Packet> all_packets;
    // Parse all original packet strings into Packet objects
    for (const std::string& s : packet_strings) {
        all_packets.push_back(parse_string_to_packet(s));
    }

    // Add the two divider packets
    Packet divider_2 = parse_string_to_packet("[[2]]");
    Packet divider_6 = parse_string_to_packet("[[6]]");
    all_packets.push_back(divider_2);
    all_packets.push_back(divider_6);

    // Sort all packets using the custom comparison logic
    // The lambda provides the strict weak ordering required by std::sort:
    // it returns true if 'a' should come before 'b'.
    std::sort(all_packets.begin(), all_packets.end(), 
              [](const Packet& a, const Packet& b) {
                  return compare_packets(a, b) == IN_ORDER;
              });

    long long decoder_key = 1; // Initialize with 1 for multiplication

    // Find the 1-based indices of the divider packets in the sorted list
    for (size_t i = 0; i < all_packets.size(); ++i) {
        // Use compare_packets to check for equality (result EQUAL)
        if (compare_packets(all_packets[i], divider_2) == EQUAL) {
            decoder_key *= (i + 1); // Multiply by 1-based index
        }
        if (compare_packets(all_packets[i], divider_6) == EQUAL) {
            decoder_key *= (i + 1); // Multiply by 1-based index
        }
    }
    std::cout << "Part 2: Decoder key: " << decoder_key << std::endl;

    return 0;
}
