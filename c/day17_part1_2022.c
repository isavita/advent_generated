
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define WIDTH 7
#define NUM_ROCKS 2022
#define NUM_SHAPES 5
#define MAX_ROCK_POINTS 5
#define MAX_JET_LEN 12000 // Sufficiently large for typical inputs
#define CHAMBER_HEIGHT 4000 // Estimated sufficient height for 2022 rocks

typedef struct {
    int x, y;
} Point;

// Rock shapes defined relative to their bottom-left origin point
const Point rock_shapes[NUM_SHAPES][MAX_ROCK_POINTS] = {
    {{0,0}, {1,0}, {2,0}, {3,0}, {-1,-1}}, // Horizontal Line (-) - 4 points
    {{1,0}, {0,1}, {1,1}, {2,1}, {1,2}}, // Plus (+) - 5 points
    {{0,0}, {1,0}, {2,0}, {2,1}, {2,2}}, // Inverted L (⌝) - 5 points
    {{0,0}, {0,1}, {0,2}, {0,3}, {-1,-1}}, // Vertical Line (|) - 4 points
    {{0,0}, {1,0}, {0,1}, {1,1}, {-1,-1}}  // Square (■) - 4 points
};

// Number of points in each rock shape
const int rock_sizes[NUM_SHAPES] = {4, 5, 5, 4, 4};

// Chamber: 0 = empty, 1 = rock. Indexing: chamber[y][x]
// y=0 represents the floor.
unsigned char chamber[CHAMBER_HEIGHT][WIDTH];
int highest_y = 0; // Tracks the highest occupied coordinate (y)

// Checks if a single point is valid (within bounds and not colliding)
bool is_valid(int x, int y) {
    if (x < 0 || x >= WIDTH || y <= 0 || y >= CHAMBER_HEIGHT) {
        return false; // Out of bounds (y<=0 hits floor)
    }
    if (chamber[y][x]) {
        return false; // Collision with settled rock
    }
    return true;
}

// Checks if the entire rock can move in the given direction (dx, dy)
bool can_move(Point rock[], int size, int dx, int dy) {
    for (int i = 0; i < size; i++) {
        if (!is_valid(rock[i].x + dx, rock[i].y + dy)) {
            return false;
        }
    }
    return true;
}

// Moves the rock by (dx, dy)
void move_rock(Point rock[], int size, int dx, int dy) {
    for (int i = 0; i < size; i++) {
        rock[i].x += dx;
        rock[i].y += dy;
    }
}

// Settles the rock in the chamber
void settle_rock(Point rock[], int size) {
    for (int i = 0; i < size; i++) {
        chamber[rock[i].y][rock[i].x] = 1;
        if (rock[i].y > highest_y) {
            highest_y = rock[i].y;
        }
    }
}


int main() {
    char jet_pattern[MAX_JET_LEN];
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }
    if (fgets(jet_pattern, sizeof(jet_pattern), file) == NULL) {
         perror("Error reading file or file empty");
         fclose(file);
         return 1;
    }
    fclose(file);

    // Remove potential trailing newline
    jet_pattern[strcspn(jet_pattern, "\n")] = 0;
    int jet_len = strlen(jet_pattern);
    int jet_index = 0;

    // Initialize chamber floor (redundant with check y<=0, but explicit)
    // memset(chamber, 0, sizeof(chamber)); // Global variables are initialized to 0 anyway

    Point current_rock[MAX_ROCK_POINTS];

    for (int rock_num = 0; rock_num < NUM_ROCKS; rock_num++) {
        int shape_index = rock_num % NUM_SHAPES;
        int current_rock_size = rock_sizes[shape_index];

        // Initial position: x=2, y=highest_y + 4
        int start_x = 2;
        int start_y = highest_y + 4;

        // Create the current rock instance at its starting position
        for(int i = 0; i < current_rock_size; i++) {
            current_rock[i].x = start_x + rock_shapes[shape_index][i].x;
            current_rock[i].y = start_y + rock_shapes[shape_index][i].y;
        }

        while (true) {
            // 1. Jet Push
            char jet = jet_pattern[jet_index % jet_len];
            jet_index++;
            int dx_jet = (jet == '>') ? 1 : -1;

            if (can_move(current_rock, current_rock_size, dx_jet, 0)) {
                move_rock(current_rock, current_rock_size, dx_jet, 0);
            }

            // 2. Move Down
            if (can_move(current_rock, current_rock_size, 0, -1)) {
                move_rock(current_rock, current_rock_size, 0, -1);
            } else {
                // Cannot move down, settle the rock
                settle_rock(current_rock, current_rock_size);
                break; // Next rock
            }
        }
         // Optimization: If chamber gets too high, could shift down, but for 2022 rocks, 4000 height is likely enough.
         if (highest_y >= CHAMBER_HEIGHT - 10) { // Check if close to buffer limit
              fprintf(stderr, "Warning: Chamber height approaching limit.\n");
              // Consider implementing shifting or increasing CHAMBER_HEIGHT if this occurs.
         }
    }

    printf("%d\n", highest_y);

    return 0;
}
