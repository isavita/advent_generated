
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// --- Configuration ---
// Maximum expected dimensions of the input grid.
#define MAX_DIM 200
// File to read input from.
#define INPUT_FILE "input.txt"

// --- Padded Grid Setup ---
// We use a 1-cell padding around the grid to simplify boundary checks.
#define PADDING 1
#define GRID_DIM (MAX_DIM + 2 * PADDING)

// --- Global Data Structures ---
// Using globals is efficient for this kind of problem, avoiding passing
// large structures or multiple pointers to every function.
char grid[GRID_DIM][GRID_DIM];
bool visited[GRID_DIM][GRID_DIM];
int height = 0, width = 0;

typedef struct { int r, c; } Point;

// A simple queue for Breadth-First Search (BFS).
Point queue[GRID_DIM * GRID_DIM];

// An array to store coordinates of the current region being processed.
Point region_coords[GRID_DIM * GRID_DIM];
// A boolean grid for fast lookups if a plot is in the current region.
bool is_in_region[GRID_DIM][GRID_DIM];

// Reads the map from INPUT_FILE into the padded `grid`.
void read_input() {
    // Initialize the entire padded grid with a non-plant boundary character.
    memset(grid, '.', sizeof(grid));

    FILE *f = fopen(INPUT_FILE, "r");
    if (!f) {
        perror("Failed to open " INPUT_FILE);
        exit(EXIT_FAILURE);
    }

    char line_buf[GRID_DIM];
    while (fgets(line_buf, sizeof(line_buf), f)) {
        if (height >= MAX_DIM) {
            fprintf(stderr, "Error: Input height exceeds MAX_DIM.\n");
            break;
        }
        // Remove trailing newline characters.
        line_buf[strcspn(line_buf, "\n\r")] = 0;
        int len = strlen(line_buf);
        if (len == 0) continue;

        if (width == 0) { // First line determines grid width
            width = len;
            if (width > MAX_DIM) {
                fprintf(stderr, "Error: Input width exceeds MAX_DIM.\n");
                width = MAX_DIM;
            }
        }
        // Copy the line into the center of our padded grid.
        memcpy(&grid[height + PADDING][PADDING], line_buf, width);
        height++;
    }
    fclose(f);

    if (height == 0 || width == 0) {
        fprintf(stderr, "Error: Input file is empty or invalid.\n");
        exit(EXIT_FAILURE);
    }
}

// Helper to check if a face is exposed (neighbor is different or boundary).
// Thanks to padding, (r, c) will always be valid coordinates.
static inline bool is_exposed(int r, int c, char plant_type) {
    return grid[r][c] != plant_type;
}

// Main logic to find all regions and calculate total prices.
void solve() {
    long long total_price_part1 = 0;
    long long total_price_part2 = 0;

    // Iterate through each cell of the grid to find starting points of new regions.
    for (int r_start = PADDING; r_start < height + PADDING; r_start++) {
        for (int c_start = PADDING; c_start < width + PADDING; c_start++) {
            if (visited[r_start][c_start]) {
                continue;
            }

            // --- New Region Found: Start BFS ---
            char plant_type = grid[r_start][c_start];
            long long area = 0;
            long long perimeter = 0;
            int region_size = 0;

            int queue_head = 0, queue_tail = 0;
            queue[queue_tail++] = (Point){r_start, c_start};
            visited[r_start][c_start] = true;

            while (queue_head < queue_tail) {
                Point p = queue[queue_head++];
                area++;
                region_coords[region_size++] = p;

                int dr[] = {-1, 1, 0, 0}, dc[] = {0, 0, -1, 1}; // Up, Down, Left, Right
                for (int i = 0; i < 4; i++) {
                    int nr = p.r + dr[i], nc = p.c + dc[i];
                    if (grid[nr][nc] != plant_type) {
                        perimeter++;
                    } else if (!visited[nr][nc]) {
                        visited[nr][nc] = true;
                        queue[queue_tail++] = (Point){nr, nc};
                    }
                }
            }

            // --- Part 1: Calculate Price (Area * Perimeter) ---
            total_price_part1 += area * perimeter;

            // --- Part 2: Calculate Price (Area * Sides) ---
            long long top_faces = 0, bottom_faces = 0, left_faces = 0, right_faces = 0;
            long long top_adj = 0, bottom_adj = 0, left_adj = 0, right_adj = 0;

            // Populate is_in_region for fast O(1) checks.
            memset(is_in_region, 0, sizeof(is_in_region));
            for (int i = 0; i < region_size; i++) {
                is_in_region[region_coords[i].r][region_coords[i].c] = true;
            }

            for (int i = 0; i < region_size; i++) {
                Point p = region_coords[i];
                
                // Count total exposed faces for the region.
                if (is_exposed(p.r - 1, p.c, plant_type)) top_faces++;
                if (is_exposed(p.r + 1, p.c, plant_type)) bottom_faces++;
                if (is_exposed(p.r, p.c - 1, plant_type)) left_faces++;
                if (is_exposed(p.r, p.c + 1, plant_type)) right_faces++;

                // Count adjacencies to subtract, merging individual faces into sides.
                // We check right and down neighbors to count each pair only once.
                if (is_in_region[p.r][p.c + 1]) { // Horizontal adjacency
                    if (is_exposed(p.r - 1, p.c, plant_type) && is_exposed(p.r - 1, p.c + 1, plant_type)) top_adj++;
                    if (is_exposed(p.r + 1, p.c, plant_type) && is_exposed(p.r + 1, p.c + 1, plant_type)) bottom_adj++;
                }
                if (is_in_region[p.r + 1][p.c]) { // Vertical adjacency
                    if (is_exposed(p.r, p.c - 1, plant_type) && is_exposed(p.r + 1, p.c - 1, plant_type)) left_adj++;
                    if (is_exposed(p.r, p.c + 1, plant_type) && is_exposed(p.r + 1, p.c + 1, plant_type)) right_adj++;
                }
            }

            long long sides = (top_faces - top_adj) + (bottom_faces - bottom_adj) + (left_faces - left_adj) + (right_faces - right_adj);
            total_price_part2 += area * sides;
        }
    }

    // --- Output Results ---
    printf("Part 1 Total Price: %lld\n", total_price_part1);
    printf("Part 2 Total Price: %lld\n", total_price_part2);
}

int main() {
    read_input();
    solve();
    return 0;
}
