
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_LINES 200
#define MAX_LINE_LEN 200
#define HEAP_INIT_CAPACITY 1024
#define MIN_STRAIGHT 0 // Part 1 constraint
#define MAX_STRAIGHT 3 // Part 1 constraint

// --- Coord Struct and Helpers ---
typedef struct {
    int x;
    int y;
} Coord;

static inline Coord coord_add(Coord a, Coord b) {
    return (Coord){a.x + b.x, a.y + b.y};
}

static inline Coord coord_subtract(Coord a, Coord b) {
    return (Coord){a.x - b.x, a.y - b.y};
}

static inline int coord_equal(Coord a, Coord b) {
    return a.x == b.x && a.y == b.y;
}

static inline Coord coord_opposite(Coord a) {
    return (Coord){-a.x, -a.y};
}

// --- Grid Struct and Helpers ---
typedef struct {
    int width;
    int height;
    int **data;
} Grid;

// --- State for A* Search ---
// State includes position, direction entered from, and steps in that direction
typedef struct {
    Coord coord;
    Coord dir; // Direction vector {dx, dy}
    int num_straight;
} State;

// --- Priority Queue (Min-Heap) ---
typedef struct {
    int priority; // Total estimated cost (cost + heuristic)
    int cost;     // Cost so far
    State state;
} HeapNode;

typedef struct {
    HeapNode *nodes;
    int size;
    int capacity;
} PriorityQueue;

PriorityQueue* create_pq(int capacity) {
    PriorityQueue *pq = (PriorityQueue*)malloc(sizeof(PriorityQueue));
    if (!pq) return NULL;
    pq->nodes = (HeapNode*)malloc(capacity * sizeof(HeapNode));
    if (!pq->nodes) {
        free(pq);
        return NULL;
    }
    pq->size = 0;
    pq->capacity = capacity;
    return pq;
}

void destroy_pq(PriorityQueue *pq) {
    if (!pq) return;
    free(pq->nodes);
    free(pq);
}

static inline void swap_nodes(HeapNode *a, HeapNode *b) {
    HeapNode temp = *a;
    *a = *b;
    *b = temp;
}

void heapify_up(PriorityQueue *pq, int index) {
    while (index > 0) {
        int parent_index = (index - 1) / 2;
        if (pq->nodes[index].priority < pq->nodes[parent_index].priority) {
            swap_nodes(&pq->nodes[index], &pq->nodes[parent_index]);
            index = parent_index;
        } else {
            break;
        }
    }
}

void heapify_down(PriorityQueue *pq, int index) {
    int size = pq->size;
    while (1) {
        int left_child_index = 2 * index + 1;
        int right_child_index = 2 * index + 2;
        int smallest_index = index;

        if (left_child_index < size && pq->nodes[left_child_index].priority < pq->nodes[smallest_index].priority) {
            smallest_index = left_child_index;
        }
        if (right_child_index < size && pq->nodes[right_child_index].priority < pq->nodes[smallest_index].priority) {
            smallest_index = right_child_index;
        }

        if (smallest_index != index) {
            swap_nodes(&pq->nodes[index], &pq->nodes[smallest_index]);
            index = smallest_index;
        } else {
            break;
        }
    }
}

int push_pq(PriorityQueue *pq, HeapNode node) {
    if (pq->size == pq->capacity) {
        int new_capacity = pq->capacity * 2;
        HeapNode *new_nodes = (HeapNode*)realloc(pq->nodes, new_capacity * sizeof(HeapNode));
        if (!new_nodes) {
            // Handle realloc failure - maybe return error or exit
             fprintf(stderr, "Error: Failed to reallocate memory for priority queue\n");
             return 0; // Indicate failure
        }
        pq->nodes = new_nodes;
        pq->capacity = new_capacity;
    }
    pq->nodes[pq->size] = node;
    pq->size++;
    heapify_up(pq, pq->size - 1);
    return 1; // Indicate success
}


HeapNode pop_pq(PriorityQueue *pq) {
    // Assumes pq is not empty, check before calling
    HeapNode root_node = pq->nodes[0];
    pq->nodes[0] = pq->nodes[pq->size - 1];
    pq->size--;
    heapify_down(pq, 0);
    return root_node;
}

static inline int is_empty_pq(PriorityQueue *pq) {
    return pq->size == 0;
}

// --- A* Helper Functions ---
static inline int heuristic(Coord a, Coord b) {
    return abs(a.x - b.x) + abs(a.y - b.y);
}

// Map direction vector to an index (0: Up, 1: Left, 2: Down, 3: Right, 4: Start/None)
static inline int dir_to_index(Coord dir) {
    if (dir.y == -1) return 0; // Up
    if (dir.x == -1) return 1; // Left
    if (dir.y == 1)  return 2; // Down
    if (dir.x == 1)  return 3; // Right
    return 4; // Special index for initial state {0,0} direction
}

// --- A* Implementation ---
int a_star_constrained(Grid *grid, Coord start, Coord goal, int min_straight, int max_straight) {
    int width = grid->width;
    int height = grid->height;
    Coord directions[] = {{0, -1}, {-1, 0}, {0, 1}, {1, 0}}; // N, W, S, E

    // Cost array: cost_so_far[y][x][dir_index][num_straight]
    // Dimensions: height, width, 5 directions (0-3 cardinal, 4 start), max_straight + 1
    int**** cost_so_far = (int****)malloc(height * sizeof(int***));
    if (!cost_so_far) return -1; // Allocation failure

    for (int y = 0; y < height; ++y) {
        cost_so_far[y] = (int***)malloc(width * sizeof(int**));
         if (!cost_so_far[y]) { /* Handle error, free previous */ return -1; }
        for (int x = 0; x < width; ++x) {
            cost_so_far[y][x] = (int**)malloc(5 * sizeof(int*)); // 5 directions
             if (!cost_so_far[y][x]) { /* Handle error, free previous */ return -1; }
            for (int d = 0; d < 5; ++d) {
                cost_so_far[y][x][d] = (int*)malloc((max_straight + 1) * sizeof(int));
                if (!cost_so_far[y][x][d]) { /* Handle error, free previous */ return -1; }
                for (int s = 0; s <= max_straight; ++s) {
                    cost_so_far[y][x][d][s] = INT_MAX;
                }
            }
        }
    }

    PriorityQueue *frontier = create_pq(HEAP_INIT_CAPACITY);
     if (!frontier) { /* Handle error, free cost_so_far */ return -1; }

    // Initial state: at start, no direction yet, 0 steps
    State start_state = {start, {0, 0}, 0};
    int start_dir_idx = dir_to_index(start_state.dir); // Should be 4
    cost_so_far[start.y][start.x][start_dir_idx][0] = 0;

    HeapNode start_node = {heuristic(start, goal), 0, start_state};
    if (!push_pq(frontier, start_node)) { /* Handle error */ return -1; };

    int min_final_cost = INT_MAX;

    while (!is_empty_pq(frontier)) {
        HeapNode current_node = pop_pq(frontier);
        State current_state = current_node.state;
        int current_cost = current_node.cost;
        Coord current_coord = current_state.coord;
        Coord current_dir = current_state.dir;
        int current_num_straight = current_state.num_straight;
        int current_dir_idx = dir_to_index(current_dir);

        // If we already found a better path to this exact state, skip
        if (current_cost > cost_so_far[current_coord.y][current_coord.x][current_dir_idx][current_num_straight]) {
            continue;
        }

        // Check if goal reached (optimization: can potentially stop earlier)
        // Note: The constraint is on *entering* the node.
        if (coord_equal(current_coord, goal)) {
             if (current_cost < min_final_cost) { // Find minimum among all ways to reach the goal
                 min_final_cost = current_cost;
             }
             // Don't return immediately, another path might reach goal cheaper later.
             // Continue processing, but maybe prune if current_cost > min_final_cost?
        }

         // If already worse than the best found goal state, prune this path
        if (current_cost >= min_final_cost) {
            continue;
        }


        // Explore neighbors
        for (int i = 0; i < 4; ++i) {
            Coord new_dir = directions[i];
            Coord neighbor_coord = coord_add(current_coord, new_dir);

            // Bounds check
            if (neighbor_coord.x < 0 || neighbor_coord.x >= width || neighbor_coord.y < 0 || neighbor_coord.y >= height) {
                continue;
            }

            // Constraint: Cannot reverse direction (unless at start)
            if (!coord_equal(current_dir, (Coord){0, 0}) && coord_equal(new_dir, coord_opposite(current_dir))) {
                continue;
            }

            int new_num_straight = coord_equal(new_dir, current_dir) ? current_num_straight + 1 : 1;
            int new_dir_idx = dir_to_index(new_dir);

            // Constraint: Maximum straight line steps
            if (new_num_straight > max_straight) {
                continue;
            }

            // Constraint: Minimum straight line steps before turning
            // If this is a turn (!coord_equal(new_dir, current_dir))
            // AND we are not at the start (current_dir != {0,0})
            // AND we haven't moved enough steps (current_num_straight < min_straight)
            // THEN this turn is invalid.
            if (!coord_equal(current_dir, (Coord){0,0}) && !coord_equal(new_dir, current_dir) && current_num_straight < min_straight) {
                 continue;
             }


            int new_cost = current_cost + grid->data[neighbor_coord.y][neighbor_coord.x];

            if (new_cost < cost_so_far[neighbor_coord.y][neighbor_coord.x][new_dir_idx][new_num_straight]) {
                cost_so_far[neighbor_coord.y][neighbor_coord.x][new_dir_idx][new_num_straight] = new_cost;
                int priority = new_cost + heuristic(neighbor_coord, goal);
                State neighbor_state = {neighbor_coord, new_dir, new_num_straight};
                HeapNode neighbor_node = {priority, new_cost, neighbor_state};
                if (!push_pq(frontier, neighbor_node)) { /* Handle error */ goto cleanup; }
            }
        }
    }

cleanup:
    // Free allocated memory
    destroy_pq(frontier);
    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            for (int d = 0; d < 5; ++d) {
                free(cost_so_far[y][x][d]);
            }
            free(cost_so_far[y][x]);
        }
        free(cost_so_far[y]);
    }
    free(cost_so_far);

    return (min_final_cost == INT_MAX) ? -1 : min_final_cost;
}


// --- Main Execution ---
int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    char lines[MAX_LINES][MAX_LINE_LEN];
    int height = 0;
    int width = 0;

    while (height < MAX_LINES && fgets(lines[height], MAX_LINE_LEN, file)) {
        // Remove trailing newline character if present
        lines[height][strcspn(lines[height], "\n")] = 0;
        if (height == 0) {
            width = strlen(lines[height]);
        } else if (strlen(lines[height]) != width) {
             fprintf(stderr, "Error: Inconsistent line length in input file.\n");
             fclose(file);
             return 1;
        }
        height++;
    }
    fclose(file);

    if (height == 0 || width == 0) {
        fprintf(stderr, "Error: Empty input file or failed to read.\n");
        return 1;
    }

    // Create and populate the grid
    Grid grid;
    grid.width = width;
    grid.height = height;
    grid.data = (int**)malloc(height * sizeof(int*));
     if (!grid.data) { perror("Memory allocation failed"); return 1; }

    for (int y = 0; y < height; ++y) {
        grid.data[y] = (int*)malloc(width * sizeof(int));
         if (!grid.data[y]) { perror("Memory allocation failed"); /* free previous rows */ return 1; }
        for (int x = 0; x < width; ++x) {
            grid.data[y][x] = lines[y][x] - '0'; // Convert char '0'-'9' to int 0-9
        }
    }

    Coord start = {0, 0};
    Coord goal = {width - 1, height - 1};

    int result = a_star_constrained(&grid, start, goal, MIN_STRAIGHT, MAX_STRAIGHT);

    printf("%d\n", result);

    // Free grid memory
    for (int y = 0; y < height; ++y) {
        free(grid.data[y]);
    }
    free(grid.data);

    return 0;
}
