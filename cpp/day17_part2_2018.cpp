
#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <algorithm>
#include <regex>
#include <tuple> // For std::tuple

// Using char for cell state for small memory footprint and easy debugging display
enum CellState : char {
    SAND = '.',
    CLAY = '#',
    FLOWING_WATER = '|',
    SETTLED_WATER = '~',
    SPRING = '+'
};

// Global grid and boundaries
std::vector<std::vector<char>> grid;
int min_x_abs_clay, max_x_abs_clay; // Absolute x coordinates of clay veins
int min_y_abs_clay, max_y_abs_clay; // Absolute y coordinates of clay veins

int grid_min_x_coord;        // Smallest absolute x coordinate represented in grid
int grid_width;              // Number of columns in grid (grid_max_x_coord - grid_min_x_coord + 1)
int grid_height;             // Number of rows in grid (grid_max_y_coord - grid_min_y_coord + 1)

// Spring position (absolute)
const int SPRING_X = 500;
const int SPRING_Y = 0;

// The `fill` function will be recursive
void fill(int x_abs, int y_abs);

void solve() {
    std::ifstream infile("input.txt");
    if (!infile.is_open()) {
        std::cerr << "Error: Could not open input.txt" << std::endl;
        return;
    }
    std::string line;

    // Initialize min/max boundaries for clay, starting with spring's X for horizontal bounds
    min_x_abs_clay = SPRING_X;
    max_x_abs_clay = SPRING_X;
    min_y_abs_clay = 2000000; // Arbitrary large value to find true min_y from clay
    max_y_abs_clay = 0;   // Arbitrary small value to find true max_y from clay

    // Store parsed clay data to place on grid after determining dimensions
    std::vector<std::tuple<int, int, int, char>> clay_data; // (fixed_coord_val, range_start, range_end, fixed_type)

    // Regex to parse input lines like "x=495, y=2..7" or "y=7, x=495..501"
    std::regex re("([xy])=(\\d+), ([xy])=(\\d+)..(\\d+)");

    while (std::getline(infile, line)) {
        std::smatch sm;
        if (std::regex_match(line, sm, re)) {
            char fixed_coord_type = sm[1].str()[0];
            int fixed_coord_val = std::stoi(sm[2].str());
            // char range_coord_type = sm[3].str()[0]; // Not explicitly used
            int range_start = std::stoi(sm[4].str());
            int range_end = std::stoi(sm[5].str());

            if (fixed_coord_type == 'x') {
                // e.g., x=fixed_val, y=range_start..range_end
                clay_data.emplace_back(fixed_coord_val, range_start, range_end, 'x');
            } else { // fixed_coord_type == 'y'
                // e.g., y=fixed_val, x=range_start..range_end
                clay_data.emplace_back(fixed_coord_val, range_start, range_end, 'y');
            }
        }
    }

    // Iterate through the stored `clay_data` to find true min/max after parsing all input
    for (const auto& data : clay_data) {
        int fixed_coord_val, range_start, range_end;
        char fixed_type;
        std::tie(fixed_coord_val, range_start, range_end, fixed_type) = data;
        if (fixed_type == 'x') {
            min_x_abs_clay = std::min(min_x_abs_clay, fixed_coord_val);
            max_x_abs_clay = std::max(max_x_abs_clay, fixed_coord_val);
            min_y_abs_clay = std::min(min_y_abs_clay, range_start);
            max_y_abs_clay = std::max(max_y_abs_clay, range_end);
        } else { // fixed_type == 'y'
            min_x_abs_clay = std::min(min_x_abs_clay, range_start);
            max_x_abs_clay = std::max(max_x_abs_clay, range_end);
            min_y_abs_clay = std::min(min_y_abs_clay, fixed_coord_val);
            max_y_abs_clay = std::max(max_y_abs_clay, fixed_coord_val);
        }
    }

    // Determine grid boundaries for simulation:
    // x: need space for water to flow around clay, expand by 2 units for safety
    grid_min_x_coord = min_x_abs_clay - 2;
    int grid_max_x_coord = max_x_abs_clay + 2;
    // y: starts at 0 (spring), extends down to max_y_abs_clay.
    // Need +1 for `y+1` checks for water falling off the bottommost clay.
    int grid_min_y_coord = 0;             // Spring Y is at 0
    int grid_max_y_coord = max_y_abs_clay + 1; // One row below deepest clay for fall-off check

    grid_width = grid_max_x_coord - grid_min_x_coord + 1;
    grid_height = grid_max_y_coord - grid_min_y_coord + 1;

    grid.assign(grid_height, std::vector<char>(grid_width, SAND));

    // Place clay onto the grid
    for (const auto& data : clay_data) {
        int fixed_coord_val, range_start, range_end;
        char fixed_type;
        std::tie(fixed_coord_val, range_start, range_end, fixed_type) = data;

        if (fixed_type == 'x') {
            int x_abs = fixed_coord_val;
            for (int y_abs = range_start; y_abs <= range_end; ++y_abs) {
                // Ensure y_abs is within grid_height. It always will be due to grid_max_y_coord definition.
                grid[y_abs][x_abs - grid_min_x_coord] = CLAY;
            }
        } else { // fixed_type == 'y'
            int y_abs = fixed_coord_val;
            for (int x_abs = range_start; x_abs <= range_end; ++x_abs) {
                grid[y_abs][x_abs - grid_min_x_coord] = CLAY;
            }
        }
    }

    // Place spring symbol (for display only, not part of flow simulation logic)
    grid[SPRING_Y][SPRING_X - grid_min_x_coord] = SPRING;

    // Start water flow simulation from the tile directly below the spring
    fill(SPRING_X, SPRING_Y + 1);

    // Count tiles for Part 1 & Part 2
    long long water_reached_count = 0;
    long long water_settled_count = 0;

    // The problem specifies to count tiles with y-coordinate within the range of clay veins
    for (int y_abs = min_y_abs_clay; y_abs <= max_y_abs_clay; ++y_abs) {
        for (int x_abs = grid_min_x_coord; x_abs <= grid_max_x_coord; ++x_abs) {
            // Convert absolute x_abs to grid column index gx
            int gx = x_abs - grid_min_x_coord;
            // Ensure gx is within grid bounds before accessing (should be, by design of grid_min/max_x_coord)
            if (gx >= 0 && gx < grid_width) {
                char cell = grid[y_abs][gx];
                if (cell == FLOWING_WATER || cell == SETTLED_WATER) {
                    water_reached_count++;
                }
                if (cell == SETTLED_WATER) {
                    water_settled_count++;
                }
            }
        }
    }
    
    std::cout << "Part 1: Total water reached: " << water_reached_count << std::endl;
    std::cout << "Part 2: Settled water: " << water_settled_count << std::endl;

    // Optional: Print grid for debugging small examples
    /*
    for (int y = 0; y < grid_height; ++y) {
        // Add y-axis labels for small examples
        if (grid_height < 100) std::cout << (y < 10 ? " " : "") << (y < 100 ? " " : "") << y << " ";
        else std::cout << y << " ";
        for (int x = 0; x < grid_width; ++x) {
            std::cout << grid[y][x];
        }
        std::cout << std::endl;
    }
    */
}

// Recursive function to simulate water flow
void fill(int x_abs, int y_abs) {
    // Convert absolute coordinates to grid indices
    int gx = x_abs - grid_min_x_coord;
    int gy = y_abs;

    // Base Case 1: Water flows off the bottom of the simulated grid.
    if (gy >= grid_height) {
        return;
    }
    // Base Case 2: Water flows outside the horizontal bounds of the simulated grid.
    if (gx < 0 || gx >= grid_width) {
        return;
    }

    // Base Case 3: Water hits clay, settled water, or the spring source.
    // Cannot flow into these.
    if (grid[gy][gx] == CLAY || grid[gy][gx] == SETTLED_WATER || grid[gy][gx] == SPRING) {
        return;
    }

    // Mark current cell as flowing water
    grid[gy][gx] = FLOWING_WATER;

    // Step 1: Try to flow down
    fill(x_abs, y_abs + 1);

    // Step 2: After attempting to flow down, check state of cell below (x_abs, y_abs).
    // If water below is still flowing or it fell off the grid, then water at (x_abs, y_abs) cannot settle.
    // It remains '|' as water just passes through it.
    if (gy + 1 >= grid_height || grid[gy+1][gx] == SAND || grid[gy+1][gx] == FLOWING_WATER) {
        return;
    }

    // Step 3: Water at (x_abs, y_abs) has a solid base (clay or settled water) below it.
    // Now, try to spread horizontally.
    bool blocked_left = false;
    int actual_left_fill_bound = x_abs; // This will be the leftmost x-coordinate to fill, inclusive

    for (int curr_x_abs = x_abs - 1; ; --curr_x_abs) {
        int gx_curr = curr_x_abs - grid_min_x_coord;
        
        // If we hit the simulated grid's left boundary, water escapes.
        if (gx_curr < 0) {
            blocked_left = false;
            break;
        }
        // If we hit clay, it's blocked.
        if (grid[gy][gx_curr] == CLAY) {
            blocked_left = true;
            actual_left_fill_bound = curr_x_abs + 1; // Wall at curr_x_abs, water fills up to curr_x_abs + 1
            break;
        }
        
        // Check if water would fall down from this horizontal extension point
        if (gy + 1 >= grid_height || grid[gy+1][gx_curr] == SAND || grid[gy+1][gx_curr] == FLOWING_WATER) {
            fill(curr_x_abs, y_abs + 1); // Recurse down to simulate water falling from this point
            // Crucial re-check: After the recursive call returns, is there still a path for water to escape downwards?
            if (gy + 1 >= grid_height || grid[gy+1][gx_curr] == SAND || grid[gy+1][gx_curr] == FLOWING_WATER) {
                blocked_left = false; // Yes, water escapes. This side is not blocked.
                break;
            }
        }
        
        // Mark as flowing water and continue left
        grid[gy][gx_curr] = FLOWING_WATER;
        actual_left_fill_bound = curr_x_abs; // Extend the fillable boundary further left
    }

    bool blocked_right = false;
    int actual_right_fill_bound = x_abs; // This will be the rightmost x-coordinate to fill, inclusive

    for (int curr_x_abs = x_abs + 1; ; ++curr_x_abs) {
        int gx_curr = curr_x_abs - grid_min_x_coord;
        
        // If we hit the simulated grid's right boundary, water escapes.
        if (gx_curr >= grid_width) {
            blocked_right = false;
            break;
        }
        // If we hit clay, it's blocked.
        if (grid[gy][gx_curr] == CLAY) {
            blocked_right = true;
            actual_right_fill_bound = curr_x_abs - 1; // Wall at curr_x_abs, water fills up to curr_x_abs - 1
            break;
        }

        // Check if water would fall down from this horizontal extension point
        if (gy + 1 >= grid_height || grid[gy+1][gx_curr] == SAND || grid[gy+1][gx_curr] == FLOWING_WATER) {
            fill(curr_x_abs, y_abs + 1);
            if (gy + 1 >= grid_height || grid[gy+1][gx_curr] == SAND || grid[gy+1][gx_curr] == FLOWING_WATER) {
                blocked_right = false; // Yes, water escapes.
                break;
            }
        }
        grid[gy][gx_curr] = FLOWING_WATER;
        actual_right_fill_bound = curr_x_abs;
    }

    // Step 4: If blocked on both sides, this entire horizontal section can become settled water.
    if (blocked_left && blocked_right) {
        // Convert all relevant cells in this row to SETTLED_WATER
        for (int curr_x = actual_left_fill_bound; curr_x <= actual_right_fill_bound; ++curr_x) {
            grid[gy][curr_x - grid_min_x_coord] = SETTLED_WATER;
        }
        // Step 5: Propagate 'settled' status upwards.
        // The water at (x_abs, y_abs) has now settled. If water flowed from (x_abs, y_abs-1) to here,
        // then (x_abs, y_abs-1) might also be able to settle now.
        // We ensure y_abs is above the spring level (y=0) to prevent going infinitely up.
        if (y_abs > SPRING_Y) {
            fill(x_abs, y_abs - 1);
        }
    }
}

int main() {
    // Optimize C++ standard streams for competitive programming
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    solve();

    return 0;
}

