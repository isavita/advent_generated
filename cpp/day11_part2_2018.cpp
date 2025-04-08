
#include <iostream>
#include <vector>
#include <fstream>
#include <string>
#include <limits>
#include <tuple> // Not strictly needed now, but good for potential struct/tuple return

// Define grid size as a constant
const int GRID_SIZE = 300;

// Use a 2D C-style array for potential performance gains with fixed size
// Size is GRID_SIZE + 1 to handle 1-based indexing easily for cumulative sum
int cumulative_grid[GRID_SIZE + 1][GRID_SIZE + 1];

// Function to calculate power level for a single cell (1-based coordinates)
inline int calculate_power_level(int x, int y, int serial_number) {
    int rack_id = x + 10;
    int power_level = rack_id * y;
    power_level += serial_number;
    power_level *= rack_id;
    // Integer division equivalent to Python's //
    power_level = (power_level / 100) % 10;
    power_level -= 5;
    return power_level;
}

// Function to calculate the total power of a square using the cumulative grid
inline int calculate_total_power(int x, int y, int size) {
    // Coordinates for the summed-area table calculation (1-based)
    int x1 = x - 1;
    int y1 = y - 1;
    int x2 = x + size - 1;
    int y2 = y + size - 1;
    // Calculate sum using inclusion-exclusion principle on the cumulative grid
    return cumulative_grid[x2][y2]
           - cumulative_grid[x1][y2]
           - cumulative_grid[x2][y1]
           + cumulative_grid[x1][y1];
}

int main() {
    // Optimize C++ standard streams
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);
    std::cout.tie(NULL);

    std::ifstream infile("input.txt");
    int serial_number;
    infile >> serial_number;
    infile.close();

    // --- Build the cumulative grid (summed-area table) ---
    // Initialize 0th row and column to 0 (simplifies cumulative calculation)
    // Note: C-style global arrays are zero-initialized by default, but explicit is clearer
     for (int i = 0; i <= GRID_SIZE; ++i) {
         cumulative_grid[i][0] = 0;
         cumulative_grid[0][i] = 0;
     }

    // Calculate power levels and fill the cumulative grid simultaneously
    for (int x = 1; x <= GRID_SIZE; ++x) {
        for (int y = 1; y <= GRID_SIZE; ++y) {
            int power = calculate_power_level(x, y, serial_number);
            // Standard formula for 2D cumulative sum (using 1-based indices)
            cumulative_grid[x][y] = power
                                    + cumulative_grid[x - 1][y]
                                    + cumulative_grid[x][y - 1]
                                    - cumulative_grid[x - 1][y - 1];
        }
    }

    // --- Find the largest total power square ---
    int max_power = std::numeric_limits<int>::min();
    int best_x = 0;
    int best_y = 0;
    int best_size = 0;

    // Iterate through all possible square sizes
    for (int size = 1; size <= GRID_SIZE; ++size) {
        // Iterate through all possible top-left corners (x, y) for the current size
        // Max x/y coordinate is GRID_SIZE - size + 1
        for (int x = 1; x <= GRID_SIZE - size + 1; ++x) {
            for (int y = 1; y <= GRID_SIZE - size + 1; ++y) {
                int total_power = calculate_total_power(x, y, size);
                if (total_power > max_power) {
                    max_power = total_power;
                    best_x = x;
                    best_y = y;
                    best_size = size;
                }
            }
        }
    }

    // Print the result in the required format
    std::cout << best_x << "," << best_y << "," << best_size << std::endl;

    return 0;
}
