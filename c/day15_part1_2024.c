
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LEN 1024

char **grid = NULL;
int rows = 0;
int cols = 0;
char *moves = NULL;
int moves_len = 0;
int moves_cap = 0;
int robot_r, robot_c;

// Forward declaration
int push_boxes(int r, int c, int dr, int dc);

int push_boxes(int r, int c, int dr, int dc) {
    int nr = r + dr;
    int nc = c + dc;

    // Check bounds
    if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
        return 0; // Cannot push out of bounds
    }

    char next_cell = grid[nr][nc];

    if (next_cell == '#') {
        return 0; // Cannot push into a wall
    }
    if (next_cell == 'O') {
        // Recursively try to push the next box
        if (!push_boxes(nr, nc, dr, dc)) {
            return 0; // Chain reaction failed
        }
        // After successful recursive push, the grid[nr][nc] should be '.'
        // Re-read the cell in case the recursive call modified it (though it shouldn't be needed here)
        // next_cell = grid[nr][nc];
    }
    // If next_cell is '.' or was 'O' but successfully pushed
    if (grid[nr][nc] == '.') {
         grid[nr][nc] = 'O'; // Move the current box
         grid[r][c] = '.';   // Clear the old box position
         return 1; // Push successful
    }
    
    // Should not happen if logic is correct (e.g., pushing into '@')
    return 0; 
}


int main() {
    FILE *f = fopen("input.txt", "r");
    if (f == NULL) {
        perror("Error opening input.txt");
        return 1;
    }

    char line[MAX_LINE_LEN];
    int reading_map = 1;
    int current_grid_cap = 0;

    while (fgets(line, sizeof(line), f)) {
        // Remove trailing newline character
        line[strcspn(line, "\n")] = 0;
        int len = strlen(line);
        if (len == 0) { // Empty line typically separates map and moves
             if (rows > 0) { // Only switch if we've actually read some grid
                 reading_map = 0;
             }
            continue;
        }

        if (reading_map) {
            // Heuristic from Python: if '#' is present, it's likely map
            if (strchr(line, '#') || strchr(line, '@') || strchr(line, 'O') || strchr(line, '.')) {
                 if (rows >= current_grid_cap) {
                    current_grid_cap = (current_grid_cap == 0) ? 8 : current_grid_cap * 2;
                    char **new_grid = realloc(grid, current_grid_cap * sizeof(char *));
                    if (!new_grid) {
                       perror("Failed to realloc grid rows");
                       // Free previously allocated memory before exiting
                       for(int i = 0; i < rows; ++i) free(grid[i]);
                       free(grid);
                       free(moves);
                       fclose(f);
                       return 1;
                    }
                    grid = new_grid;
                 }

                if (cols == 0) {
                    cols = len;
                } else if (len > cols) { 
                     // Handle potentially wider lines (pad shorter ones later if needed, though this problem seems to assume rectangular)
                     // For simplicity here, we'll just update cols if a wider line is found.
                     // A more robust solution might pad existing rows or error out.
                     cols = len; 
                }


                grid[rows] = malloc((cols + 1) * sizeof(char)); // +1 for null terminator
                 if (!grid[rows]) {
                     perror("Failed to allocate grid row");
                      // Free previously allocated memory
                      for(int i = 0; i < rows; ++i) free(grid[i]);
                      free(grid);
                      free(moves);
                      fclose(f);
                      return 1;
                 }
                 // Copy and potentially pad shorter lines with spaces (or assume input is rectangular)
                 strncpy(grid[rows], line, cols); 
                 // Ensure null termination and handle padding if needed (Python implicitly pads)
                 for(int k=len; k<cols; ++k) grid[rows][k] = ' '; // Pad with spaces if line is shorter
                 grid[rows][cols] = '\0'; 

                 // Find robot initial position while reading grid
                 char *robot_ptr = strchr(grid[rows], '@');
                 if (robot_ptr) {
                     robot_r = rows;
                     robot_c = (int)(robot_ptr - grid[rows]);
                 }
                 rows++;
            } else {
                // Line doesn't seem like part of the map, assume moves start
                reading_map = 0;
                // Append this line to moves
                 int line_len = strlen(line);
                 if (moves_len + line_len + 1 > moves_cap) {
                     moves_cap = (moves_cap == 0) ? 128 : moves_cap * 2;
                     while (moves_cap < moves_len + line_len + 1) moves_cap *= 2; // Ensure enough space
                     char *new_moves = realloc(moves, moves_cap);
                     if (!new_moves) {
                         perror("Failed to realloc moves");
                          // Free allocated grid memory
                         for(int i = 0; i < rows; ++i) free(grid[i]);
                         free(grid);
                         free(moves);
                         fclose(f);
                         return 1;
                     }
                     moves = new_moves;
                     if (moves_len == 0) moves[0] = '\0'; // Initialize if first time
                 }
                 strcat(moves, line);
                 moves_len += line_len;
            }
        } else {
             // Append line to moves
             int line_len = strlen(line);
              if (moves_len + line_len + 1 > moves_cap) {
                  moves_cap = (moves_cap == 0) ? 128 : moves_cap * 2;
                  while (moves_cap < moves_len + line_len + 1) moves_cap *= 2;
                  char *new_moves = realloc(moves, moves_cap);
                   if (!new_moves) {
                       perror("Failed to realloc moves");
                        // Free allocated grid memory
                       for(int i = 0; i < rows; ++i) free(grid[i]);
                       free(grid);
                       free(moves);
                       fclose(f);
                       return 1;
                   }
                  moves = new_moves;
                  if (moves_len == 0) moves[0] = '\0'; // Initialize
              }
             strcat(moves, line);
             moves_len += line_len;
        }
    }
    fclose(f);
    
    if (!grid || rows == 0 || cols == 0 || !moves) {
        fprintf(stderr, "Error: Invalid input format (missing grid or moves).\n");
         // Free any allocated memory
         for(int i = 0; i < rows; ++i) free(grid[i]);
         free(grid);
         free(moves);
        return 1;
    }


    // Process moves
    for (int i = 0; i < moves_len; ++i) {
        char move = moves[i];
        int dr = 0, dc = 0;

        switch (move) {
            case '^': dr = -1; break;
            case 'v': dr = 1;  break;
            case '<': dc = -1; break;
            case '>': dc = 1;  break;
            default: continue; // Ignore invalid move characters
        }

        int nr = robot_r + dr;
        int nc = robot_c + dc;

        // Check bounds for robot's next position
        if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
            continue;
        }

        char next_cell = grid[nr][nc];

        if (next_cell == '#') {
            continue; // Hit wall
        } else if (next_cell == 'O') {
            // Try to push the box
            if (!push_boxes(nr, nc, dr, dc)) {
                continue; // Push failed
            }
             // If push succeeded, the grid[nr][nc] is now '.'
             // Fall through to move the robot into the now empty space
        }
        
        // If next_cell is '.' or was 'O' and push succeeded
         grid[robot_r][robot_c] = '.'; // Clear old robot position
         grid[nr][nc] = '@';           // Move robot to new position
         robot_r = nr;
         robot_c = nc;
    }

    // Calculate final sum
    long long total_sum = 0;
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            if (grid[r][c] == 'O') {
                total_sum += (long long)r * 100 + c;
            }
        }
    }

    printf("%lld\n", total_sum);

    // Free allocated memory
    for (int i = 0; i < rows; ++i) {
        free(grid[i]);
    }
    free(grid);
    free(moves);

    return 0;
}
