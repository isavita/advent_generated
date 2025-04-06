
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

// Assuming max grid dimensions based on typical puzzle inputs
#define MAX_DIM 50
#define WALL_THRESHOLD 400 // Nodes with used space > this are considered walls for the hole

typedef struct {
    int x;
    int y;
} Point;

typedef struct {
    int used;
    int avail;
} Node;

// --- Global Grid Data ---
Node nodes[MAX_DIM][MAX_DIM];
int width = 0;
int height = 0;

// --- Queue for BFS ---
Point queue[MAX_DIM * MAX_DIM];
int head = 0;
int tail = 0;

void enqueue(Point p) {
    queue[tail++] = p;
}

Point dequeue() {
    return queue[head++];
}

int is_queue_empty() {
    return head == tail;
}

void reset_queue() {
    head = 0;
    tail = 0;
}

// --- BFS Implementation ---
int moves(Point goal, Point from_pos, Point to_pos) {
    if (from_pos.x == to_pos.x && from_pos.y == to_pos.y) {
        return 0;
    }

    int depth[MAX_DIM][MAX_DIM];
    for (int i = 0; i < width; ++i) {
        for (int j = 0; j < height; ++j) {
            depth[i][j] = -1;
        }
    }

    reset_queue();

    depth[from_pos.x][from_pos.y] = 0;
    enqueue(from_pos);

    Point neighbors[4] = {{0, 1}, {0, -1}, {1, 0}, {-1, 0}};

    while (!is_queue_empty()) {
        Point p = dequeue();

        if (p.x == to_pos.x && p.y == to_pos.y) {
            return depth[p.x][p.y];
        }

        int current_depth = depth[p.x][p.y];

        for (int i = 0; i < 4; ++i) {
            Point next_pos = {p.x + neighbors[i].x, p.y + neighbors[i].y};

            // Check bounds
            if (next_pos.x < 0 || next_pos.y < 0 || next_pos.x >= width || next_pos.y >= height) {
                continue;
            }
            // Check if it's the goal node (can't move hole into goal data)
            if (next_pos.x == goal.x && next_pos.y == goal.y) {
                continue;
            }
            // Check if it's a wall
            if (nodes[next_pos.x][next_pos.y].used > WALL_THRESHOLD) {
                continue;
            }
            // Check if visited
            if (depth[next_pos.x][next_pos.y] == -1) {
                depth[next_pos.x][next_pos.y] = current_depth + 1;
                enqueue(next_pos);
            }
        }
    }

    return -1; // Should not happen if a path exists
}

// --- Main Logic ---
int minmoves() {
    Point goal = {width - 1, 0};
    Point hole = {-1, -1};

    // Find initial hole
    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            if (nodes[x][y].used == 0) {
                hole.x = x;
                hole.y = y;
                goto found_hole;
            }
        }
    }
found_hole:
    if (hole.x == -1) return -1; // Error: no hole found

    int moves_sum = 0;
    while (goal.x != 0 || goal.y != 0) {
        // Target position for the hole (adjacent to the left of the goal)
        Point hole_target = {goal.x - 1, goal.y};

        // Moves to bring the hole to the target position
        int m = moves(goal, hole, hole_target);
        if (m == -1) return -1; // No path for hole
        moves_sum += m;
        hole = hole_target; // Hole is now adjacent to goal

        // Move the goal data into the hole (1 move)
        moves_sum += 1;

        // Swap goal and hole positions for the next iteration
        Point temp = goal;
        goal = hole;
        hole = temp; // The old goal position is now the hole
    }
    return moves_sum;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    char line[256];
    // Skip header lines
    if (!fgets(line, sizeof(line), file)) goto read_error;
    if (!fgets(line, sizeof(line), file)) goto read_error;

    int max_x = 0, max_y = 0;
    int x, y, used, avail, size, perc; // Add size/perc for sscanf

    // Read node data
    while (fgets(line, sizeof(line), file)) {
         // Format: /dev/grid/node-x{X}-y{Y}  {Size}T {Used}T {Avail}T {Use%}
         if (sscanf(line, "/dev/grid/node-x%d-y%d %dT %dT %dT %d%%", &x, &y, &size, &used, &avail, &perc) == 6) {
            if (x >= MAX_DIM || y >= MAX_DIM) {
                 fprintf(stderr, "Error: Grid dimensions exceed MAX_DIM (%d)\n", MAX_DIM);
                 fclose(file);
                 return 1;
            }
            nodes[x][y].used = used;
            nodes[x][y].avail = avail;
            if (x > max_x) max_x = x;
            if (y > max_y) max_y = y;
        } else {
            // Handle potential parsing errors or different line formats if necessary
            fprintf(stderr, "Warning: Could not parse line: %s", line);
        }
    }

    width = max_x + 1;
    height = max_y + 1;

    fclose(file);

    int result = minmoves();
    if (result != -1) {
        printf("%d\n", result);
    } else {
        fprintf(stderr, "Error calculating moves (no path or no hole?).\n");
        return 1;
    }

    return 0;

read_error:
    fprintf(stderr, "Error reading from input.txt\n");
    fclose(file);
    return 1;
}

