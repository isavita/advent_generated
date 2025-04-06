
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Define maximum grid dimensions (adjust if necessary)
#define MAX_H 500
#define MAX_W 500

typedef struct {
    int y;
    int x;
} Coord;

// Global storage for grid and antenna locations
char grid[MAX_H][MAX_W + 2]; // +2 for potential newline and null terminator
Coord antenna_coords[256][MAX_H * MAX_W]; // Store coords per character ASCII value
int antenna_counts[256] = {0};
bool is_antinode[MAX_H][MAX_W] = {false}; // Track antinode locations
int h = 0, w = 0;

// Function to calculate Greatest Common Divisor (GCD) using Euclidean algorithm
// Handles negative numbers by taking absolute values. Uses long long for safety.
long long gcd(long long a, long long b) {
    a = llabs(a); // Absolute value for long long
    b = llabs(b);
    while (b) {
        a %= b;
        // Swap a and b
        long long temp = a;
        a = b;
        b = temp;
    }
    return a;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        // If input.txt doesn't exist or can't be opened, assume empty grid.
        printf("0\n");
        return 0;
        // Or: perror("Error opening input.txt"); return 1;
    }

    // Read grid from file
    while (h < MAX_H && fgets(grid[h], MAX_W + 2, fp)) {
        // Remove trailing newline/carriage return
        grid[h][strcspn(grid[h], "\r\n")] = 0;
        if (strlen(grid[h]) == 0) continue; // Skip empty lines

        if (h == 0) {
            w = strlen(grid[0]);
            if (w > MAX_W) {
                 // Handle line too long error if necessary, for now truncate
                 w = MAX_W;
                 grid[0][w] = 0;
            }
        }
         // Ensure consistent width (optional, depends on problem constraints)
         // if (strlen(grid[h]) != w) { /* Handle inconsistent width */ }
        h++;
    }
    fclose(fp);

    if (h == 0 || w == 0) {
        printf("0\n"); // Empty grid
        return 0;
    }

    // Identify antenna locations and group by frequency (char)
    for (int y = 0; y < h; ++y) {
        for (int x = 0; x < w; ++x) {
            if (grid[y][x] != '.') {
                unsigned char freq_idx = (unsigned char)grid[y][x];
                // Check array bounds before adding (simple safeguard)
                if(antenna_counts[freq_idx] < MAX_H * MAX_W) {
                   int idx = antenna_counts[freq_idx]++;
                   antenna_coords[freq_idx][idx] = (Coord){y, x};
                } else {
                   // Error: Too many antennas of this type for allocated space
                   // Depending on constraints, might need dynamic allocation or larger fixed size
                }
            }
        }
    }

    long long antinode_count = 0;

    // Iterate through each frequency that has at least two antennas
    for (int f = 0; f < 256; ++f) {
        int n = antenna_counts[f];
        if (n < 2) continue;

        Coord* coords = antenna_coords[f];

        // Consider all pairs of antennas for this frequency
        for (int i = 0; i < n; ++i) {
            for (int j = i + 1; j < n; ++j) {
                Coord A = coords[i];
                Coord B = coords[j];

                long long dy = (long long)B.y - A.y;
                long long dx = (long long)B.x - A.x;

                // Calculate simplified slope (sy, sx)
                long long common_divisor = gcd(dy, dx);
                long long sy = dy / common_divisor;
                long long sx = dx / common_divisor;

                // Normalize direction: sx >= 0, and if sx == 0, then sy > 0
                if (sx < 0 || (sx == 0 && sy < 0)) {
                    sx = -sx;
                    sy = -sy;
                }

                // Line equation: sy*x - sx*y = c
                // Calculate c using point A: c = sy*A.x - sx*A.y
                long long c = sy * A.x - sx * A.y;

                // --- Identify and mark antinodes on this line within the grid ---

                if (sy == 0) { // Horizontal line: -sx * y = c => sx * y = -c
                    if (sx == 0) continue; // Should not happen for distinct points A, B
                    if (llabs(c) % sx == 0) { // Check divisibility safely
                        long long y_line = -c / sx;
                        if (y_line >= 0 && y_line < h) {
                            int iy = (int)y_line;
                            for (int ix = 0; ix < w; ++ix) {
                                if (!is_antinode[iy][ix]) {
                                    is_antinode[iy][ix] = true;
                                    antinode_count++;
                                }
                            }
                        }
                    }
                } else if (sx == 0) { // Vertical line: sy * x = c
                     if (sy == 0) continue; // Should not happen
                     if (llabs(c) % sy == 0) { // Check divisibility safely
                        long long x_line = c / sy;
                         if (x_line >= 0 && x_line < w) {
                             int ix = (int)x_line;
                             for (int iy = 0; iy < h; ++iy) {
                                 if (!is_antinode[iy][ix]) {
                                     is_antinode[iy][ix] = true;
                                     antinode_count++;
                                 }
                             }
                         }
                     }
                } else { // Diagonal line: sy * x - sx * y = c
                     // Iterate through grid rows (y) and calculate corresponding x
                     for (int iy = 0; iy < h; ++iy) {
                         // Check if (c + sx * iy) is divisible by sy
                         long long num = c + sx * (long long)iy;
                         if (num % sy == 0) {
                             long long x_line = num / sy;
                             // Check if calculated x is within grid bounds
                             if (x_line >= 0 && x_line < w) {
                                 int ix = (int)x_line;
                                  if (!is_antinode[iy][ix]) {
                                      is_antinode[iy][ix] = true;
                                      antinode_count++;
                                  }
                             }
                         }
                     }
                }
            }
        }
    }

    printf("%lld\n", antinode_count);

    return 0;
}
