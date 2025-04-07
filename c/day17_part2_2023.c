
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_WIDTH 200
#define MAX_HEIGHT 200
#define MAX_STEPS 10
#define HEAP_CAPACITY (MAX_WIDTH * MAX_HEIGHT * 4 * (MAX_STEPS + 1)) // Generous estimate

typedef enum { N, S, E, W } Direction;

// State for Dijkstra's algorithm
typedef struct {
    int cost;
    int r, c;        // Row, Column
    Direction dir;   // Direction entered this cell
    int steps;       // Consecutive steps in this direction
} State;

// Priority Queue (Min-Heap) implementation
State heap[HEAP_CAPACITY];
int heap_size = 0;

void swap(State* a, State* b) {
    State temp = *a;
    *a = *b;
    *b = temp;
}

void heap_push(State s) {
    if (heap_size >= HEAP_CAPACITY) {
        fprintf(stderr, "Heap capacity exceeded\n");
        exit(1); // Should not happen with generous capacity
    }
    heap[heap_size] = s;
    int i = heap_size++;
    while (i > 0) {
        int p = (i - 1) / 2;
        if (heap[p].cost <= heap[i].cost) break;
        swap(&heap[p], &heap[i]);
        i = p;
    }
}

State heap_pop() {
    if (heap_size <= 0) {
        fprintf(stderr, "Heap underflow\n");
        exit(1); // Should not happen if goal is reachable
    }
    State top = heap[0];
    heap[0] = heap[--heap_size];
    int i = 0;
    while (1) {
        int l = 2 * i + 1;
        int r = 2 * i + 2;
        int smallest = i;
        if (l < heap_size && heap[l].cost < heap[smallest].cost) smallest = l;
        if (r < heap_size && heap[r].cost < heap[smallest].cost) smallest = r;
        if (smallest == i) break;
        swap(&heap[i], &heap[smallest]);
        i = smallest;
    }
    return top;
}

// Grid and dimensions
char grid[MAX_HEIGHT][MAX_WIDTH + 2]; // +2 for potential newline and null terminator
int height = 0;
int width = 0;

// Visited/Distance array: dist[row][col][direction][steps]
int dist[MAX_HEIGHT][MAX_WIDTH][4][MAX_STEPS + 1];

// Direction vectors (dr, dc) for N, S, E, W
int dr[] = {-1, 1, 0, 0};
int dc[] = {0, 0, 1, -1};

// Dijkstra implementation
int solve(int min_steps_turn, int max_steps_straight) {
    // Initialize distances to infinity
    for (int r = 0; r < height; ++r) {
        for (int c = 0; c < width; ++c) {
            for (int d = 0; d < 4; ++d) {
                for (int s = 0; s <= max_steps_straight; ++s) {
                    dist[r][c][d][s] = INT_MAX;
                }
            }
        }
    }

    heap_size = 0; // Reset heap

    // Push initial possible moves from (0,0)
    // We consider the cost is incurred when *entering* a cell.
    // Start requires moving East or South.

    // Move East to (0, 1) if possible
    if (width > 1) {
        int initial_cost_e = grid[0][1] - '0';
        State start_e = {initial_cost_e, 0, 1, E, 1};
        dist[0][1][E][1] = initial_cost_e;
        heap_push(start_e);
    }
    // Move South to (1, 0) if possible
    if (height > 1) {
        int initial_cost_s = grid[1][0] - '0';
        State start_s = {initial_cost_s, 1, 0, S, 1};
        dist[1][0][S][1] = initial_cost_s;
        heap_push(start_s);
    }


    while (heap_size > 0) {
        State current = heap_pop();
        int cost = current.cost;
        int r = current.r;
        int c = current.c;
        Direction dir = current.dir;
        int steps = current.steps;

        // If cost is already higher than recorded, skip
        if (cost > dist[r][c][dir][steps]) {
            continue;
        }

        // Check if goal reached (with minimum steps constraint for part 2)
        if (r == height - 1 && c == width - 1 && steps >= min_steps_turn) {
             return cost;
        }

        // Explore next possible moves (straight and turns)
        for (Direction next_dir = N; next_dir <= W; ++next_dir) {
            // Rule 1: Cannot reverse direction
            if ((dir == N && next_dir == S) || (dir == S && next_dir == N) ||
                (dir == E && next_dir == W) || (dir == W && next_dir == E)) {
                continue;
            }

            int next_r = r + dr[next_dir];
            int next_c = c + dc[next_dir];

            // Rule 2: Stay within bounds
            if (next_r < 0 || next_r >= height || next_c < 0 || next_c >= width) {
                continue;
            }

            int new_cost = cost + (grid[next_r][next_c] - '0');
            int new_steps;

            if (next_dir == dir) {
                // Continue straight
                // Rule 3: Cannot exceed max steps straight
                if (steps >= max_steps_straight) continue;
                new_steps = steps + 1;
            } else {
                // Turn
                // Rule 4: Must take min steps before turning
                if (steps < min_steps_turn) continue;
                new_steps = 1; // Reset steps after turn
            }

            // If this path is better than previously found path to this state
            if (new_cost < dist[next_r][next_c][next_dir][new_steps]) {
                dist[next_r][next_c][next_dir][new_steps] = new_cost;
                State next_state = {new_cost, next_r, next_c, next_dir, new_steps};
                heap_push(next_state);
            }
        }
    }

    return -1; // Should not happen if goal is reachable
}


int main() {
    FILE *infile = fopen("input.txt", "r");
    if (!infile) {
        perror("Error opening input.txt");
        return 1;
    }

    // Read grid
    height = 0;
    while (height < MAX_HEIGHT && fgets(grid[height], MAX_WIDTH + 2, infile)) {
        // Remove trailing newline if present
        grid[height][strcspn(grid[height], "\n")] = 0;
        if (height == 0) {
            width = strlen(grid[0]);
        }
         // Basic validation: ensure all lines have same width
        if (strlen(grid[height]) != width) {
            fprintf(stderr, "Inconsistent line width at line %d\n", height + 1);
            fclose(infile);
            return 1;
        }
        height++;
    }
    fclose(infile);

    if (height == 0 || width == 0) {
        fprintf(stderr, "Input grid is empty\n");
        return 1;
    }

    // Part 1
    int result1 = solve(1, 3);
    printf("%d\n", result1);

    // Part 2
    int result2 = solve(4, 10);
    printf("%d\n", result2);


    return 0;
}
