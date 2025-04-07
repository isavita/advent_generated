
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_R 200
#define MAX_C 1000
#define EXTRA_SPACE 250 // Increased buffer slightly just in case
#define START_COL 500

char matrix[MAX_R][MAX_C];
int max_r = 0;
int min_c = INT_MAX;
int max_c_coord = 0; // Max column coordinate before offset

// Helper function for min/max
int imin(int a, int b) { return a < b ? a : b; }
int imax(int a, int b) { return a > b ? a : b; }

// Draw line segment in the grid using adjusted coordinates
void draw_line(int r1, int c1, int r2, int c2, int offset_c) {
    int adj_c1 = c1 - min_c + offset_c;
    int adj_c2 = c2 - min_c + offset_c;

    // Ensure coordinates are within bounds after adjustment (basic sanity check)
    if (r1 >= MAX_R || r2 >= MAX_R || adj_c1 < 0 || adj_c1 >= MAX_C || adj_c2 < 0 || adj_c2 >= MAX_C) {
         fprintf(stderr, "Warning: Coordinates out of bounds after adjustment.\n");
         // Potentially increase MAX_R/MAX_C if this occurs
         return;
    }


    if (r1 == r2) { // Horizontal line
        int start_c = imin(adj_c1, adj_c2);
        int end_c = imax(adj_c1, adj_c2);
        for (int c = start_c; c <= end_c; c++) {
            matrix[r1][c] = '#';
        }
    } else if (adj_c1 == adj_c2) { // Vertical line
        int start_r = imin(r1, r2);
        int end_r = imax(r1, r2);
        for (int r = start_r; r <= end_r; r++) {
            matrix[r][adj_c1] = '#';
        }
    }
    // Diagonal lines are not expected based on the problem description
}

// Simulates dropping one unit of sand.
// Returns 1 if sand comes to rest, 0 if the source is blocked.
int drop_sand(int origin_c) {
    int r = 0, c = origin_c;

    // Check if source is already blocked
    if (matrix[r][c] == 'o') {
        return 0;
    }

    while (r < MAX_R - 1) {
        // Check bounds for diagonal checks
        int can_go_left = (c > 0);
        int can_go_right = (c < MAX_C - 1);

        if (matrix[r + 1][c] == '.') { // Try down
            r++;
        } else if (can_go_left && matrix[r + 1][c - 1] == '.') { // Try down-left
            r++;
            c--;
        } else if (can_go_right && matrix[r + 1][c + 1] == '.') { // Try down-right
            r++;
            c++;
        } else { // Cannot move, sand rests
            matrix[r][c] = 'o';
            return 1;
        }
    }

    // Should not be reached if floor is correctly placed within bounds
    fprintf(stderr, "Error: Sand reached bottom boundary unexpectedly.\n");
    return 0; // Indicate failure or source block technically
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        return 1;
    }

    // Initialize grid
    memset(matrix, '.', sizeof(matrix));

    char line[2048];
    int first_pass_min_c = INT_MAX;
    int first_pass_max_r = 0;
    int first_pass_max_c = 0;

    // First pass: determine coordinate bounds
    while (fgets(line, sizeof(line), fp)) {
        char *segment = strtok(line, " -> ");
        while (segment != NULL) {
            int r, c;
            if (sscanf(segment, "%d,%d", &c, &r) == 2) {
                first_pass_min_c = imin(first_pass_min_c, c);
                first_pass_max_r = imax(first_pass_max_r, r);
                first_pass_max_c = imax(first_pass_max_c, c);
            }
            segment = strtok(NULL, " -> \n");
        }
    }
    min_c = first_pass_min_c;
    max_r = first_pass_max_r;
    max_c_coord = first_pass_max_c; // Store max original column coordinate


    // Check if dimensions are sufficient
     int required_rows = max_r + 3; // Need space for the floor
     int required_cols_approx = (max_c_coord - min_c + 1) + 2 * EXTRA_SPACE;
     if (required_rows >= MAX_R || START_COL - min_c + EXTRA_SPACE >= MAX_C || required_cols_approx >= MAX_C ) {
         fprintf(stderr, "Error: MAX_R (%d) or MAX_C (%d) might be too small.\n", MAX_R, MAX_C);
         fprintf(stderr, "Needed Rows: ~%d, Needed Cols: ~%d, Origin Col Index: %d\n", required_rows, required_cols_approx, START_COL - min_c + EXTRA_SPACE);
         fclose(fp);
         return 1;
     }


    // Rewind file for second pass
    rewind(fp);

    // Second pass: draw rock formations
    while (fgets(line, sizeof(line), fp)) {
        int prev_r = -1, prev_c = -1;
        char *segment = strtok(line, " -> ");
        while (segment != NULL) {
            int r, c;
            if (sscanf(segment, "%d,%d", &c, &r) == 2) {
                if (prev_r != -1) {
                    draw_line(prev_r, prev_c, r, c, EXTRA_SPACE);
                }
                prev_r = r;
                prev_c = c;
            }
            segment = strtok(NULL, " -> \n");
        }
    }
    fclose(fp);

    // Add the floor
    int floor_r = max_r + 2;
    if (floor_r >= MAX_R) {
         fprintf(stderr, "Error: Floor level %d exceeds MAX_R %d.\n", floor_r, MAX_R);
         return 1;
    }
    for (int c = 0; c < MAX_C; c++) {
        matrix[floor_r][c] = '#';
    }

    // Determine the adjusted starting column for sand
    int origin_c = START_COL - min_c + EXTRA_SPACE;
    if(origin_c < 0 || origin_c >= MAX_C) {
        fprintf(stderr, "Error: Origin column %d is out of bounds [0, %d).\n", origin_c, MAX_C);
        return 1;
    }

    // Simulate sand falling
    int sand_count = 0;
    while (drop_sand(origin_c)) {
        sand_count++;
    }

    // Print the result
    printf("%d\n", sand_count);

    return 0;
}
