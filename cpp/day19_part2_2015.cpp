
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <random>
#include <chrono>
#include <set>

// Helper function to replace all occurrences of a substring (non-overlapping)
// This version builds the result string piece by piece.
std::string replace_all(std::string str, const std::string& from, const std::string& to) {
    if (from.empty())
        return str;
    std::string result;
    // Estimate capacity to avoid reallocations, might need adjustment based on typical replacement sizes
    result.reserve(str.length());
    std::string::size_type start_pos = 0;
    std::string::size_type pos;
    while ((pos = str.find(from, start_pos)) != std::string::npos) {
        // Append the part of the string before the match
        result.append(str, start_pos, pos - start_pos);
        // Append the replacement string
        result.append(to);
        // Move the starting position past the found substring
        start_pos = pos + from.length();
    }
    // Append the remaining part of the original string after the last match
    result.append(str, start_pos, std::string::npos);
    return result;
}

// Helper function to count occurrences of a pattern (non-overlapping)
int count_occurrences(const std::string& text, const std::string& pattern) {
     if (pattern.empty()) return 0; // Cannot find an empty pattern meaningfully
    int count = 0;
    std::string::size_type pos = 0;
    // Repeatedly find the pattern starting from the position after the last find
    while ((pos = text.find(pattern, pos)) != std::string::npos) {
        ++count;
        // Move search position past the current match to avoid overlapping counts
        pos += pattern.length();
    }
    return count;
}


int main() {
    // Optimize C++ standard input/output streams
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream infile("input.txt");
    if (!infile.is_open()) {
        // Per requirements, no error message to stderr
        return 1; // Indicate failure to open file
    }

    std::unordered_map<std::string, std::string> product_to_reactant;
    std::set<std::string> unique_products; // Use a set to store unique products encountered
    std::string line;
    std::string starting_molecule;
    bool reading_rules = true; // Flag to track if we are reading rules or the molecule

    // Read the input file line by line
    while (std::getline(infile, line)) {
        if (line.empty()) { // An empty line separates the rules from the starting molecule
            reading_rules = false;
            continue; // Skip the empty line
        }

        if (reading_rules) {
            // Parse the rule format: "Reactant => Product"
            std::size_t arrow_pos = line.find(" => ");
            if (arrow_pos != std::string::npos) {
                std::string reactant = line.substr(0, arrow_pos);
                std::string product = line.substr(arrow_pos + 4); // Length of " => " is 4
                // Store the reverse mapping: Product -> Reactant
                product_to_reactant[product] = reactant;
                // Add the product to the set of unique products
                unique_products.insert(product);
            }
        } else {
            // After the empty line, the next line is the starting molecule
            starting_molecule = line;
            // Assuming only one line for the molecule, we can stop reading
            break;
        }
    }
    infile.close(); // Close the input file

    // Convert the set of unique products into a vector to allow shuffling
    std::vector<std::string> all_products(unique_products.begin(), unique_products.end());

    std::string mol = starting_molecule; // Current molecule state
    int steps = 0; // Counter for the number of replacement steps

    // Setup a random number generator using system clock for seed
    unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
    std::mt19937 g(seed);

    // Main loop: reduce the molecule back to "e"
    while (mol != "e") {
        bool change_made_this_iteration = false;

        // Iterate through the available products (potential reductions)
        for (const std::string& prod : all_products) {
             // Check if the current product exists in the molecule string
             std::string::size_type first_pos = mol.find(prod);
             if (first_pos != std::string::npos) {
                 // If found, count how many times this specific reduction can be applied
                 int count = count_occurrences(mol, prod);
                 // Get the reactant (the string to replace the product with)
                 const std::string& react = product_to_reactant.at(prod); // Use .at() for bounds checking (optional)
                 // Replace all occurrences of the product with the reactant
                 mol = replace_all(mol, prod, react);
                 // Increment the step count by the number of replacements made
                 steps += count;
                 change_made_this_iteration = true;
                 // As per Python logic, only perform one type of replacement per iteration
                 break;
             }
        }

        // If no replacement rule could be applied in this pass through the products
        if (!change_made_this_iteration) {
             // This check prevents resetting if we successfully reached "e"
             if (mol == "e") break;

             // We are stuck; reset the molecule and step count, then shuffle the product order
             std::shuffle(all_products.begin(), all_products.end(), g);
             mol = starting_molecule; // Reset to the original molecule
             steps = 0; // Reset step count
             // Continue to the next iteration of the while loop with the shuffled order
        }
    }

    // Print the final result (total steps)
    std::cout << steps << std::endl;

    return 0; // Indicate successful execution
}
