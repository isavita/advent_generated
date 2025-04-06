
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// --- Constants ---
// Increased limits for potentially larger grids/periods
#define MAX_H 200
#define MAX_W 200
#define MAX_BLIZZARDS 10000
// LCM can grow, ensure sufficient size. LCM(148, 149) approx 22000
#define MAX_PERIOD 30000
#define MAX_QUEUE_SIZE 4000000 // Heuristic, adjust if needed

// --- Structs ---
typedef struct {
    int x, y;
} Point;

typedef struct {
    int x, y, dir_idx; // 0:>, 1:<, 2:v, 3:^
} Blizzard;

typedef struct {
    int x, y, t;
} State;

// --- Global Variables ---
// Grid and Walls
bool walls[MAX_H][MAX_W];
int height, width;

// Blizzards
Blizzard initial_blizzards[MAX_BLIZZARDS];
int num_blizzards = 0;
bool (*blizzard_positions)[MAX_H][MAX_W] = NULL; // [period][y][x]

// BFS Queue and Visited Set
State queue[MAX_QUEUE_SIZE];
int queue_head = 0;
int queue_tail = 0;
// Dynamically allocate visited array to avoid stack overflow
bool (*visited)[MAX_W][MAX_PERIOD] = NULL; // [y][x][t % period]

Point start_pos, end_pos;
int period;

// Directions: Wait, Right, Left, Down, Up
const int dx[] = {0, 1, -1, 0, 0};
const int dy[] = {0, 0, 0, 1, -1};
const char dir_chars[] = {'>', '<', 'v', '^'};

// --- Utility Functions ---
int gcd(int a, int b) {
    while (b) {
        a %= b;
        int temp = a;
        a = b;
        b = temp;
    }
    return a;
}

int lcm(int a, int b) {
    if (a == 0 || b == 0) return 0;
    long long res = (long long)a / gcd(a, b) * b;
    if (res > MAX_PERIOD) {
         fprintf(stderr, "Error: Calculated LCM %lld exceeds MAX_PERIOD %d\n", res, MAX_PERIOD);
         exit(EXIT_FAILURE);
    }
    return (int)res;
}

// Positive modulo helper
int pos_mod(int i, int n) {
    return (i % n + n) % n;
}


// --- Core Logic Functions ---
void read_input(const char *file_path) {
    FILE *file = fopen(file_path, "r");
    if (!file) {
        perror("Error opening input file");
        exit(EXIT_FAILURE);
    }

    char line[MAX_W + 2]; // +1 for newline, +1 for null terminator
    height = 0;
    width = 0;
    memset(walls, 0, sizeof(walls));
    num_blizzards = 0;

    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove newline
        if (width == 0) {
            width = strlen(line);
        }
        for (int x = 0; x < width; ++x) {
            if (line[x] == '#') {
                walls[height][x] = true;
            } else if (line[x] != '.') {
                 if (num_blizzards >= MAX_BLIZZARDS) {
                     fprintf(stderr, "Error: Exceeded MAX_BLIZZARDS\n");
                     exit(EXIT_FAILURE);
                 }
                initial_blizzards[num_blizzards].x = x;
                initial_blizzards[num_blizzards].y = height;
                for(int k=0; k<4; ++k) {
                    if(line[x] == dir_chars[k]) {
                        initial_blizzards[num_blizzards].dir_idx = k;
                        break;
                    }
                }
                num_blizzards++;
            }
        }
        height++;
         if (height >= MAX_H) {
             fprintf(stderr, "Error: Exceeded MAX_H\n");
             exit(EXIT_FAILURE);
         }
    }
    fclose(file);

     if (width <= 2 || height <= 2) {
        fprintf(stderr, "Error: Grid dimensions too small (%dx%d)\n", width, height);
        exit(EXIT_FAILURE);
    }
}

void find_start_end() {
    for (int x = 0; x < width; ++x) {
        if (!walls[0][x]) {
            start_pos = (Point){x, 0};
            break;
        }
    }
    for (int x = 0; x < width; ++x) {
        if (!walls[height - 1][x]) {
            end_pos = (Point){x, height - 1};
            break;
        }
    }
}

void precompute_blizzards() {
    int inner_width = width - 2;
    int inner_height = height - 2;
    period = lcm(inner_width, inner_height);
    if(period == 0) { // Handle edge case from lcm
        period = 1;
    }

    blizzard_positions = calloc(period, sizeof(*blizzard_positions)); // period x H x W
     if (!blizzard_positions) {
        perror("Failed to allocate blizzard positions memory");
        exit(EXIT_FAILURE);
    }


    for (int t = 0; t < period; ++t) {
        for (int i = 0; i < num_blizzards; ++i) {
            int x = initial_blizzards[i].x;
            int y = initial_blizzards[i].y;
            int dir_idx = initial_blizzards[i].dir_idx;
            int nx = x, ny = y;

            switch (dir_idx) {
                case 0: // >
                    nx = 1 + pos_mod(x - 1 + t, inner_width);
                    break;
                case 1: // <
                    nx = 1 + pos_mod(x - 1 - t, inner_width);
                    break;
                case 2: // v
                    ny = 1 + pos_mod(y - 1 + t, inner_height);
                    break;
                case 3: // ^
                    ny = 1 + pos_mod(y - 1 - t, inner_height);
                    break;
            }
             if (ny >= 0 && ny < height && nx >= 0 && nx < width) {
                blizzard_positions[t][ny][nx] = true;
            }
        }
    }
}

int bfs(Point current_start, Point current_end, int start_time) {
    // Allocate and clear visited array for this BFS run
    visited = calloc(height, sizeof(*visited)); // H x W x Period
    if (!visited) {
        perror("Failed to allocate visited array");
        exit(EXIT_FAILURE); // Or handle error appropriately
    }

    queue_head = 0;
    queue_tail = 0;

    // Add initial state
    State initial_state = {current_start.x, current_start.y, start_time};
    queue[queue_tail++] = initial_state;
    visited[current_start.y][current_start.x][start_time % period] = true;

    while (queue_head < queue_tail) {
        if (queue_tail >= MAX_QUEUE_SIZE || queue_head >= MAX_QUEUE_SIZE) {
             fprintf(stderr, "Error: BFS Queue overflow/underflow\n");
             free(visited); // Clean up allocated memory before exiting
             exit(EXIT_FAILURE);
        }
        State current = queue[queue_head++];
        int x = current.x;
        int y = current.y;
        int t = current.t;

        int next_t = t + 1;
        int next_t_mod = next_t % period;

        for (int i = 0; i < 5; ++i) { // Iterate through 5 possible moves (including wait)
            int nx = x + dx[i];
            int ny = y + dy[i];

            // Check bounds (allow start/end positions)
             if (nx < 0 || nx >= width || ny < 0 || ny >= height) {
                continue;
            }

            // Check wall
             if (walls[ny][nx]) {
                continue;
            }

             // Check if reached end
            if (nx == current_end.x && ny == current_end.y) {
                free(visited); // Clean up visited array
                return next_t;
            }

             // Check blizzard collision at next time step
            if (blizzard_positions[next_t_mod][ny][nx]) {
                continue;
            }

             // Check if visited state
             if (visited[ny][nx][next_t_mod]) {
                 continue;
             }

             // Mark visited and enqueue
            visited[ny][nx][next_t_mod] = true;
            State next_state = {nx, ny, next_t};
            queue[queue_tail++] = next_state;
        }
    }

    free(visited); // Clean up visited array if path not found
    return -1; // No path found
}


// --- Main Function ---
int main() {
    read_input("input.txt");
    find_start_end();
    precompute_blizzards();

    int time1 = bfs(start_pos, end_pos, 0);
    if (time1 == -1) {
        fprintf(stderr, "Error: No path found for trip 1\n");
        free(blizzard_positions);
        return 1;
    }

    int time2 = bfs(end_pos, start_pos, time1);
     if (time2 == -1) {
        fprintf(stderr, "Error: No path found for trip 2\n");
        free(blizzard_positions);
        return 1;
    }

    int time3 = bfs(start_pos, end_pos, time2);
     if (time3 == -1) {
        fprintf(stderr, "Error: No path found for trip 3\n");
        free(blizzard_positions);
        return 1;
    }

    printf("%d\n", time3);

    // Cleanup
    free(blizzard_positions);
    // Note: 'visited' is freed inside bfs

    return 0;
}

