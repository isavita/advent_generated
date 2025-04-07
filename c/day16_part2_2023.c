
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_H 150
#define MAX_W 150
#define MAX_QUEUE_SIZE (MAX_H * MAX_W * 4) // Generous upper bound

char grid[MAX_H][MAX_W];
int height = 0;
int width = 0;

bool energized[MAX_H][MAX_W];
// visited[y][x][dir_index]
// dir_index: 0: right (1,0), 1: left (-1,0), 2: down (0,1), 3: up (0,-1)
bool visited[MAX_H][MAX_W][4];

typedef struct {
    int x, y, dx, dy;
} State;

State queue[MAX_QUEUE_SIZE];
int queue_head = 0;
int queue_tail = 0;

// Simple map from direction (dx, dy) to a unique index 0-3
int dir_to_index(int dx, int dy) {
    if (dx == 1 && dy == 0) return 0; // Right
    if (dx == -1 && dy == 0) return 1; // Left
    if (dx == 0 && dy == 1) return 2; // Down
    if (dx == 0 && dy == -1) return 3; // Up
    return -1; // Should not happen for valid directions
}

void enqueue(State s) {
    if (queue_tail < MAX_QUEUE_SIZE) {
        queue[queue_tail++] = s;
    } else {
         // Should not happen with MAX_QUEUE_SIZE estimate
        fprintf(stderr, "Error: Queue overflow\n");
        exit(EXIT_FAILURE);
    }
}

State dequeue() {
    if (queue_head < queue_tail) {
        return queue[queue_head++];
    } else {
        // Should not happen in correct BFS if queue isn't empty
        fprintf(stderr, "Error: Queue underflow\n");
        exit(EXIT_FAILURE);
        // Return a dummy state to avoid compiler warnings, though exit is called
        return (State){-1, -1, 0, 0};
    }
}

bool is_empty() {
    return queue_head == queue_tail;
}

void reset_simulation() {
    memset(energized, false, sizeof(energized));
    memset(visited, false, sizeof(visited));
    queue_head = 0;
    queue_tail = 0;
}

int simulate_beam(int start_x, int start_y, int start_dx, int start_dy) {
    reset_simulation();

    // Start the beam simulation from the initial position and direction
    enqueue((State){start_x, start_y, start_dx, start_dy});

    while (!is_empty()) {
        State current = dequeue();
        int x = current.x;
        int y = current.y;
        int dx = current.dx;
        int dy = current.dy;

        // Calculate the next position
        int nx = x + dx;
        int ny = y + dy;

        // Check if the next position is within bounds
        if (nx < 0 || nx >= width || ny < 0 || ny >= height) {
            continue; // Beam exits the grid
        }

        // Check if this state (position + direction) has been visited
        int dir_idx = dir_to_index(dx, dy);
        if (visited[ny][nx][dir_idx]) {
            continue; // Cycle detected
        }
        visited[ny][nx][dir_idx] = true;
        energized[ny][nx] = true; // Mark the new cell as energized

        char cell = grid[ny][nx];
        int ndx, ndy; // New directions

        if (cell == '.') {
            enqueue((State){nx, ny, dx, dy});
        } else if (cell == '/') {
            ndx = -dy; ndy = -dx;
            enqueue((State){nx, ny, ndx, ndy});
        } else if (cell == '\\') {
            ndx = dy; ndy = dx;
            enqueue((State){nx, ny, ndx, ndy});
        } else if (cell == '|') {
            if (dx != 0) { // Moving horizontally, split vertically
                enqueue((State){nx, ny, 0, 1});  // Down
                enqueue((State){nx, ny, 0, -1}); // Up
            } else { // Moving vertically, continue straight
                enqueue((State){nx, ny, dx, dy});
            }
        } else if (cell == '-') {
            if (dy != 0) { // Moving vertically, split horizontally
                enqueue((State){nx, ny, 1, 0});  // Right
                enqueue((State){nx, ny, -1, 0}); // Left
            } else { // Moving horizontally, continue straight
                enqueue((State){nx, ny, dx, dy});
            }
        }
    }

    // Count energized tiles
    int count = 0;
    for (int r = 0; r < height; ++r) {
        for (int c = 0; c < width; ++c) {
            if (energized[r][c]) {
                count++;
            }
        }
    }
    return count;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening input file");
        return 1;
    }

    height = 0;
    while (height < MAX_H && fgets(grid[height], MAX_W + 2, file)) { // +2 for potential \n and \0
        // Remove trailing newline if present
        grid[height][strcspn(grid[height], "\n")] = 0;
        if (height == 0) {
            width = strlen(grid[0]);
        }
        // Ensure all lines have the same width (optional check)
         if (strlen(grid[height]) != width && strlen(grid[height]) > 0) {
             fprintf(stderr, "Error: Inconsistent line width at line %d\n", height + 1);
             fclose(file);
             return 1;
         }
        height++;
    }
    fclose(file);

    if (height == 0 || width == 0) {
        fprintf(stderr, "Error: Empty grid or failed to read grid dimensions.\n");
        return 1;
    }


    // Part One: Start at (-1, 0) moving right (1, 0) -> first cell is (0, 0)
    // int part_one_answer = simulate_beam(-1, 0, 1, 0);
    // printf("Part One Answer: %d\n", part_one_answer); // Removed as per requirement

    // Part Two: Find the maximum energized count from any edge start
    int max_energized = 0;
    int current_energized;

    // Top edge (entering downwards)
    for (int x = 0; x < width; ++x) {
        current_energized = simulate_beam(x, -1, 0, 1);
        if (current_energized > max_energized) max_energized = current_energized;
    }

    // Bottom edge (entering upwards)
    for (int x = 0; x < width; ++x) {
        current_energized = simulate_beam(x, height, 0, -1);
        if (current_energized > max_energized) max_energized = current_energized;
    }

    // Left edge (entering rightwards)
    for (int y = 0; y < height; ++y) {
        current_energized = simulate_beam(-1, y, 1, 0);
        if (current_energized > max_energized) max_energized = current_energized;
    }

    // Right edge (entering leftwards)
    for (int y = 0; y < height; ++y) {
        current_energized = simulate_beam(width, y, -1, 0);
        if (current_energized > max_energized) max_energized = current_energized;
    }

    printf("%d\n", max_energized);

    return 0;
}
