
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <limits.h>

#define MAX_LINES 250 // Adjust if necessary

typedef struct {
    int min;
    int max;
} Boundary;

typedef struct {
    int x;
    int y;
} Position;

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char *map_lines_raw[MAX_LINES];
    char *path = NULL;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int num_rows = 0;
    int max_width = 0;

    // Read map lines
    while ((read = getline(&line, &len, file)) != -1) {
        if (line[0] == '\n' || line[0] == '\r') {
            break; // Empty line separates map and path
        }
        if (num_rows < MAX_LINES) {
             // Remove trailing newline characters
            line[strcspn(line, "\r\n")] = 0;
            map_lines_raw[num_rows] = strdup(line);
             if (!map_lines_raw[num_rows]) {
                 perror("Memory allocation failed");
                 // Free already allocated lines
                 for(int i = 0; i < num_rows; ++i) free(map_lines_raw[i]);
                 free(line);
                 fclose(file);
                 return 1;
             }
             int current_width = strlen(map_lines_raw[num_rows]);
             if (current_width > max_width) {
                 max_width = current_width;
             }
            num_rows++;
        } else {
             fprintf(stderr, "Warning: Exceeded MAX_LINES, skipping map lines.\n");
        }
    }

    // Read path line
    if ((read = getline(&line, &len, file)) != -1) {
         line[strcspn(line, "\r\n")] = 0; // Remove trailing newline
         path = strdup(line);
         if (!path) {
             perror("Memory allocation failed for path");
              for(int i = 0; i < num_rows; ++i) free(map_lines_raw[i]);
             free(line);
             fclose(file);
             return 1;
         }
    }

    free(line);
    fclose(file);

    if (num_rows == 0 || path == NULL) {
        fprintf(stderr, "Error: Invalid input format.\n");
         for(int i = 0; i < num_rows; ++i) free(map_lines_raw[i]);
         free(path);
        return 1;
    }

    int num_cols = max_width;

    // Create and pad grid
    char **grid = malloc(num_rows * sizeof(char *));
     if (!grid) {
         perror("Memory allocation failed for grid");
         for(int i = 0; i < num_rows; ++i) free(map_lines_raw[i]);
         free(path);
         return 1;
     }

    for (int i = 0; i < num_rows; ++i) {
        grid[i] = malloc((num_cols + 1) * sizeof(char)); // +1 for null terminator
        if (!grid[i]) {
             perror("Memory allocation failed for grid row");
             // Free allocated grid rows and grid itself
             for(int j = 0; j < i; ++j) free(grid[j]);
             free(grid);
             // Free raw lines and path
             for(int k = 0; k < num_rows; ++k) free(map_lines_raw[k]);
             free(path);
             return 1;
         }
        strcpy(grid[i], map_lines_raw[i]);
        int current_len = strlen(grid[i]);
        for (int j = current_len; j < num_cols; ++j) {
            grid[i][j] = ' ';
        }
        grid[i][num_cols] = '\0';
        free(map_lines_raw[i]); // Free raw line after copying
    }


    // Precompute boundaries (1-based indexing for logic, 0-based for array access)
    Boundary *row_boundaries = calloc(num_rows, sizeof(Boundary));
    Boundary *col_boundaries = calloc(num_cols, sizeof(Boundary));
    if (!row_boundaries || !col_boundaries) {
         perror("Memory allocation failed for boundaries");
          for(int i = 0; i < num_rows; ++i) free(grid[i]);
          free(grid);
          free(row_boundaries); // free even if NULL
          free(col_boundaries); // free even if NULL
          free(path);
         return 1;
     }


    for (int y = 0; y < num_rows; ++y) {
        row_boundaries[y].min = 0; // Use 0 to indicate not found yet
        for (int x = 0; x < num_cols; ++x) {
            if (grid[y][x] != ' ') {
                if (row_boundaries[y].min == 0) {
                    row_boundaries[y].min = x + 1;
                }
                row_boundaries[y].max = x + 1;
            }
        }
    }

    for (int x = 0; x < num_cols; ++x) {
         col_boundaries[x].min = 0; // Use 0 to indicate not found yet
        for (int y = 0; y < num_rows; ++y) {
            if (grid[y][x] != ' ') {
                if (col_boundaries[x].min == 0) {
                    col_boundaries[x].min = y + 1;
                }
                col_boundaries[x].max = y + 1;
            }
        }
    }

    // Find starting position (first '.' in top row, 1-based)
    Position current_pos = {0, 1};
    for (int x = 0; x < num_cols; ++x) {
        if (grid[0][x] == '.') {
            current_pos.x = x + 1;
            break;
        }
    }
    if (current_pos.x == 0) {
         fprintf(stderr, "Error: Starting position not found.\n");
         // Cleanup
         for(int i = 0; i < num_rows; ++i) free(grid[i]);
         free(grid);
         free(row_boundaries);
         free(col_boundaries);
         free(path);
         return 1;
    }


    int facing = 0; // 0: Right, 1: Down, 2: Left, 3: Up

    // Simulate movement
    char *p = path;
    while (*p) {
        if (isdigit(*p)) {
            int steps = 0;
            while (isdigit(*p)) {
                steps = steps * 10 + (*p - '0');
                p++;
            }

            for (int i = 0; i < steps; ++i) {
                Position next_pos = current_pos;
                int dx[] = {1, 0, -1, 0}; // R, D, L, U
                int dy[] = {0, 1, 0, -1}; // R, D, L, U

                next_pos.x += dx[facing];
                next_pos.y += dy[facing];

                // Handle wrapping (using 1-based indices for boundaries)
                // Adjust indices for array access (0-based)
                int current_y_idx = current_pos.y - 1;
                int current_x_idx = current_pos.x - 1;

                if (facing == 0) { // Right
                    if (next_pos.x > row_boundaries[current_y_idx].max) {
                         next_pos.x = row_boundaries[current_y_idx].min;
                    }
                } else if (facing == 2) { // Left
                     if (next_pos.x < row_boundaries[current_y_idx].min) {
                         next_pos.x = row_boundaries[current_y_idx].max;
                    }
                } else if (facing == 1) { // Down
                     if (next_pos.y > col_boundaries[current_x_idx].max) {
                         next_pos.y = col_boundaries[current_x_idx].min;
                    }
                } else if (facing == 3) { // Up
                     if (next_pos.y < col_boundaries[current_x_idx].min) {
                         next_pos.y = col_boundaries[current_x_idx].max;
                    }
                }

                 // Check tile at next position (adjusting for 0-based grid access)
                 char next_tile = grid[next_pos.y - 1][next_pos.x - 1];

                if (next_tile == '#') {
                    break; // Hit a wall
                } else if (next_tile == '.') {
                    current_pos = next_pos; // Move
                }
                 // Implicitly handles ' ' which shouldn't be reachable with correct wrapping
            }
        } else if (*p == 'L' || *p == 'R') {
            if (*p == 'R') {
                facing = (facing + 1) % 4;
            } else { // 'L'
                facing = (facing + 3) % 4; // Use +3 for correct modulo of negative numbers
            }
            p++;
        } else {
             // Should not happen with valid input
             fprintf(stderr, "Warning: Unexpected character in path: %c\n", *p);
            p++;
        }
    }

    // Compute final password
    long long password = 1000LL * current_pos.y + 4LL * current_pos.x + facing;
    printf("%lld\n", password);

    // Cleanup
    for (int i = 0; i < num_rows; ++i) {
        free(grid[i]);
    }
    free(grid);
    free(row_boundaries);
    free(col_boundaries);
    free(path);

    return 0;
}
