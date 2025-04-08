
#include <iostream>
#include <unordered_set>
#include <cstdint>

// Define constants for magic numbers to improve readability
const std::uint32_t MASK24 = 16777215; // 0xFFFFFF
const std::uint32_t MULT = 65899;
const std::uint32_t INIT_R5 = 7586220;
const std::uint32_t OR_VAL = 65536;   // 0x10000
const std::uint32_t MASK8 = 255;     // 0xFF
const std::uint32_t DIV = 256;       // 0x100

int main() {
    // Optimize C++ standard streams by decoupling from C stdio
    std::ios_base::sync_with_stdio(false);
    // Untie cin and cout for potential minor speedup (though less relevant here)
    std::cin.tie(NULL);

    std::uint32_t register5 = 0;
    std::unordered_set<std::uint32_t> seen;
    std::uint32_t last_unique = 0;

    while (true) {
        std::uint32_t register3 = register5 | OR_VAL;
        register5 = INIT_R5;

        while (true) {
            std::uint32_t register1 = register3 & MASK8;
            // Perform the calculation using 32-bit unsigned integers
            // The bitwise AND operations handle the modulo 16777216 implicitly
            register5 = register5 + register1;
            register5 &= MASK24;
            register5 *= MULT;
            register5 &= MASK24;


            if (register3 < DIV) {
                // Use find() for checking existence in unordered_set
                if (seen.find(register5) != seen.end()) {
                    // Cycle detected, print the last unique value found
                    std::cout << last_unique << std::endl;
                    return 0; // Exit program
                }
                // Insert the new unique value into the set
                seen.insert(register5);
                last_unique = register5;
                break; // Exit the inner loop
            } else {
                // Equivalent to integer division by 256
                register3 /= DIV;
            }
        }
        // Continue the outer loop until a cycle is detected and the program exits
    }

    // This part of the code is logically unreachable due to the infinite outer loop
    // and the guaranteed exit condition within the inner loop upon cycle detection.
    // A return 0; is implicitly added by the compiler if main finishes without one.
}
