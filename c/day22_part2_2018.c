
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

// Constants from Python code
#define GEOLOGIC_Y 16807LL
#define GEOLOGIC_X 48271LL
#define CAVE_MODULO 20183

// Region Types
#define TYPE_ROCKY 0
#define TYPE_WET 1
#define TYPE_NARROW 2

// Tools (using powers of 2 for bitmasks)
#define TOOL_NONE 1
#define TOOL_TORCH 2
#define TOOL_GEAR 4
#define MAX_TOOL_VALUE 4 // Max value used for indexing arrays

// Helper Constants
#define INF INT_MAX
// Heuristic padding around target for array allocation and search bound
// Increase if the solution path goes further out.
#define PADDING 100
// Max estimated heap size, adjust if needed based on PADDING and target size
#define HEAP_MAX_SIZE 4000000

// Global variables
int g_depth;
int g_targetX, g_targetY;
int g_boundX, g_boundY; // Max dimensions needed for arrays

// Caches (using -1 to indicate not calculated)
long long **g_geologic_indices_cache;
int **g_erosion_levels_cache;
// Distance map: dist[y][x][tool] -> min_time
// Tool index: 1=None, 2=Torch, 4=Gear. Array size needs to accommodate max tool value.
int ***g_distances;

// State for Dijkstra's algorithm
typedef struct {
    int time;
    int x;
    int y;
    int tool;
} State;

// Min-Heap Implementation
State g_heap[HEAP_MAX_SIZE];
int g_heap_size = 0;

void swap(State *a, State *b) {
    State temp = *a;
    *a = *b;
    *b = temp;
}

void heapify_up(int index) {
    while (index > 0) {
        int parent_index = (index - 1) / 2;
        if (g_heap[index].time < g_heap[parent_index].time) {
            swap(&g_heap[index], &g_heap[parent_index]);
            index = parent_index;
        } else {
            break;
        }
    }
}

void heapify_down(int index) {
    int left_child, right_child, smallest;
    while (1) {
        smallest = index;
        left_child = 2 * index + 1;
        right_child = 2 * index + 2;

        if (left_child < g_heap_size && g_heap[left_child].time < g_heap[smallest].time) {
            smallest = left_child;
        }
        if (right_child < g_heap_size && g_heap[right_child].time < g_heap[smallest].time) {
            smallest = right_child;
        }

        if (smallest != index) {
            swap(&g_heap[index], &g_heap[smallest]);
            index = smallest;
        } else {
            break;
        }
    }
}

void heap_push(State s) {
    if (g_heap_size >= HEAP_MAX_SIZE) {
        fprintf(stderr, "Error: Heap overflow\n");
        exit(EXIT_FAILURE);
    }
    g_heap[g_heap_size] = s;
    g_heap_size++;
    heapify_up(g_heap_size - 1);
}

State heap_pop() {
    if (g_heap_size <= 0) {
        fprintf(stderr, "Error: Heap underflow\n");
        exit(EXIT_FAILURE);
    }
    State min_state = g_heap[0];
    g_heap[0] = g_heap[g_heap_size - 1];
    g_heap_size--;
    heapify_down(0);
    return min_state;
}


// --- Cave Calculation Functions ---

long long get_geologic_index(int x, int y); // Forward declaration

int get_erosion_level(int x, int y) {
    if (x < 0 || y < 0 || x >= g_boundX || y >= g_boundY) return -1; // Invalid coordinates handled implicitly by cache check below
    if (g_erosion_levels_cache[y][x] != -1) {
        return g_erosion_levels_cache[y][x];
    }

    long long geo_index = get_geologic_index(x, y);
    if (geo_index == -1) return -1; // Should not happen if called correctly

    int level = (int)((geo_index + g_depth) % CAVE_MODULO);
    g_erosion_levels_cache[y][x] = level;
    return level;
}

long long get_geologic_index(int x, int y) {
     if (x < 0 || y < 0 || x >= g_boundX || y >= g_boundY) return -1; // Bounds check
    if (g_geologic_indices_cache[y][x] != -1) {
        return g_geologic_indices_cache[y][x];
    }

    long long index;
    if (x == 0 && y == 0) {
        index = 0;
    } else if (x == g_targetX && y == g_targetY) {
        index = 0;
    } else if (y == 0) {
        index = (long long)x * GEOLOGIC_Y;
    } else if (x == 0) {
        index = (long long)y * GEOLOGIC_X;
    } else {
        long long erosion_left = get_erosion_level(x - 1, y);
        long long erosion_up = get_erosion_level(x, y - 1);
         if (erosion_left == -1 || erosion_up == -1) { // Check dependencies calculated
             fprintf(stderr, "Error: Dependency not calculated for geo_index(%d, %d)\n", x, y);
             exit(EXIT_FAILURE); // Or handle more gracefully
             //return -1; // Indicate error
         }
        index = erosion_left * erosion_up;
    }

    g_geologic_indices_cache[y][x] = index;
    return index;
}

int get_type(int x, int y) {
    return get_erosion_level(x, y) % 3;
}

// Returns bitmask of allowed tools for a region type
int allowed_tools(int region_type) {
    switch (region_type) {
        case TYPE_ROCKY: return TOOL_GEAR | TOOL_TORCH;
        case TYPE_WET:   return TOOL_GEAR | TOOL_NONE;
        case TYPE_NARROW:return TOOL_TORCH | TOOL_NONE;
        default:         return 0; // Unknown type
    }
}

// --- Memory Management ---

void allocate_memory() {
    g_boundX = g_targetX + PADDING;
    g_boundY = g_targetY + PADDING;

    // Allocate rows (array of pointers)
    g_geologic_indices_cache = malloc(g_boundY * sizeof(long long *));
    g_erosion_levels_cache = malloc(g_boundY * sizeof(int *));
    g_distances = malloc(g_boundY * sizeof(int **));

    if (!g_geologic_indices_cache || !g_erosion_levels_cache || !g_distances) goto memory_error;

    for (int y = 0; y < g_boundY; ++y) {
        g_geologic_indices_cache[y] = malloc(g_boundX * sizeof(long long));
        g_erosion_levels_cache[y] = malloc(g_boundX * sizeof(int));
        g_distances[y] = malloc(g_boundX * sizeof(int *));

        if (!g_geologic_indices_cache[y] || !g_erosion_levels_cache[y] || !g_distances[y]) goto memory_error;

        // Initialize caches to -1
        memset(g_geologic_indices_cache[y], -1, g_boundX * sizeof(long long));
        memset(g_erosion_levels_cache[y], -1, g_boundX * sizeof(int));

        for (int x = 0; x < g_boundX; ++x) {
            // Allocate tool dimension and initialize distances to INF
            g_distances[y][x] = malloc((MAX_TOOL_VALUE + 1) * sizeof(int)); // Size 5 for indices 1, 2, 4
             if (!g_distances[y][x]) goto memory_error;
            for (int t = 0; t <= MAX_TOOL_VALUE; ++t) {
                g_distances[y][x][t] = INF;
            }
        }
    }
    return;

memory_error:
    fprintf(stderr, "Error: Memory allocation failed.\n");
    // Basic cleanup, a full cleanup function would be better
    // This part is simplified for brevity
    free(g_geologic_indices_cache);
    free(g_erosion_levels_cache);
    free(g_distances);
    exit(EXIT_FAILURE);
}

void free_memory() {
    for (int y = 0; y < g_boundY; ++y) {
        free(g_geologic_indices_cache[y]);
        free(g_erosion_levels_cache[y]);
        for (int x = 0; x < g_boundX; ++x) {
            free(g_distances[y][x]);
        }
        free(g_distances[y]);
    }
    free(g_geologic_indices_cache);
    free(g_erosion_levels_cache);
    free(g_distances);
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    if (fscanf(file, "depth: %d\ntarget: %d,%d", &g_depth, &g_targetX, &g_targetY) != 3) {
        fprintf(stderr, "Error reading input format.\n");
        fclose(file);
        return 1;
    }
    fclose(file);

    allocate_memory();

    // Pre-calculate erosion levels up to bounds needed by Dijkstra later, avoids recursion depth issues
    // Note: This is an optimization/simplification. A purely lazy calculation during Dijkstra is also possible.
    for (int y = 0; y < g_boundY; ++y) {
        for (int x = 0; x < g_boundX; ++x) {
             get_erosion_level(x, y); // calculates geo index implicitly if needed
        }
    }


    // Dijkstra initialization
    g_distances[0][0][TOOL_TORCH] = 0;
    heap_push((State){0, 0, 0, TOOL_TORCH});

    int dx[] = {0, 0, 1, -1};
    int dy[] = {1, -1, 0, 0};

    int final_time = -1;

    while (g_heap_size > 0) {
        State current = heap_pop();

        // If we found a shorter path already, skip this old state
        if (current.time > g_distances[current.y][current.x][current.tool]) {
            continue;
        }

        // Target reached?
        if (current.x == g_targetX && current.y == g_targetY && current.tool == TOOL_TORCH) {
            final_time = current.time;
            break;
        }

        // Explore neighbors (including tool changes)
        // 1. Consider changing tool at current location (cost +7)
        int current_type = get_type(current.x, current.y);
        int allowed_here = allowed_tools(current_type);
        for (int next_tool = 1; next_tool <= MAX_TOOL_VALUE; next_tool <<= 1) { // Iterate TOOL_NONE, TOOL_TORCH, TOOL_GEAR
             if ((next_tool & allowed_here) && next_tool != current.tool) {
                int new_time = current.time + 7;
                 if (new_time < g_distances[current.y][current.x][next_tool]) {
                     g_distances[current.y][current.x][next_tool] = new_time;
                     heap_push((State){new_time, current.x, current.y, next_tool});
                 }
             }
        }


        // 2. Consider moving to adjacent cells (cost +1)
        for (int i = 0; i < 4; ++i) {
            int nx = current.x + dx[i];
            int ny = current.y + dy[i];

            // Check bounds
            if (nx < 0 || ny < 0 || nx >= g_boundX || ny >= g_boundY) {
                continue;
            }

            int neighbor_type = get_type(nx, ny);
            int allowed_there = allowed_tools(neighbor_type);

            // If current tool is allowed in the neighbor cell
            if (current.tool & allowed_there) {
                int new_time = current.time + 1;
                if (new_time < g_distances[ny][nx][current.tool]) {
                    g_distances[ny][nx][current.tool] = new_time;
                    heap_push((State){new_time, nx, ny, current.tool});
                }
            }
        }
    }

    if (final_time != -1) {
        printf("%d\n", final_time);
    } else {
        printf("Target not reached.\n"); // Should not happen with valid input
    }

    free_memory();

    return 0;
}
