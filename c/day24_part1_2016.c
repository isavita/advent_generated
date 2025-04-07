
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

#define MAX_POIS 10 // Max number of points of interest (0-9)

// --- Grid and BFS Structures ---

typedef struct {
    int rows;
    int cols;
    char **data;
} Grid;

typedef struct {
    int row;
    int col;
    int distance;
} BfsNode;

typedef struct {
    BfsNode *nodes;
    int capacity;
    int size;
    int head;
    int tail;
} Queue;

// --- Function Declarations ---

Grid* read_grid(const char* filename);
void free_grid(Grid* grid);
void find_pois(Grid* grid, int poi_coords[][2], int* num_pois);
int* bfs_get_edge_weights(Grid* grid, int start_row, int start_col, int num_pois);
int dfs(int** graph, int num_pois, int current_poi, int visited_mask);
int count_set_bits(int n);

Queue* create_queue(int capacity);
void free_queue(Queue* q);
void enqueue(Queue* q, BfsNode node);
BfsNode dequeue(Queue* q);
int is_queue_empty(Queue* q);

// --- Main Logic ---

int main() {
    Grid* grid = read_grid("input.txt");
    if (!grid) {
        return 1;
    }

    int poi_coords[MAX_POIS][2];
    int num_pois = 0;
    find_pois(grid, poi_coords, &num_pois);

    if (num_pois == 0) {
        printf("No points of interest found.\n");
        free_grid(grid);
        return 1;
    }

    // Allocate graph (adjacency matrix storing distances)
    int** graph = (int**)malloc(num_pois * sizeof(int*));
    if (!graph) {
        perror("Failed to allocate graph rows");
        free_grid(grid);
        return 1;
    }
    for (int i = 0; i < num_pois; ++i) {
        graph[i] = (int*)malloc(num_pois * sizeof(int));
        if (!graph[i]) {
             perror("Failed to allocate graph columns");
             // Free previously allocated rows
             for(int k = 0; k < i; ++k) free(graph[k]);
             free(graph);
             free_grid(grid);
             return 1;
        }
    }

    // Calculate edge weights using BFS from each POI
    for (int i = 0; i < num_pois; ++i) {
        int* distances = bfs_get_edge_weights(grid, poi_coords[i][0], poi_coords[i][1], num_pois);
        if (!distances) {
             // Free graph memory
             for(int k = 0; k < num_pois; ++k) free(graph[k]);
             free(graph);
             free_grid(grid);
             return 1;
        }
        memcpy(graph[i], distances, num_pois * sizeof(int));
        free(distances);
    }

    // Solve TSP using DFS starting from POI 0
    int initial_visited_mask = 1; // Mark POI 0 as visited (1 << 0)
    int result = dfs(graph, num_pois, 0, initial_visited_mask);

    printf("%d\n", result);

    // Free resources
    for (int i = 0; i < num_pois; ++i) {
        free(graph[i]);
    }
    free(graph);
    free_grid(grid);

    return 0;
}

// --- Grid Reading ---

Grid* read_grid(const char* filename) {
    FILE* fp = fopen(filename, "r");
    if (!fp) {
        perror("Error opening file");
        return NULL;
    }

    Grid* grid = (Grid*)malloc(sizeof(Grid));
    if (!grid) {
         perror("Failed to allocate grid struct");
         fclose(fp);
         return NULL;
    }
    grid->rows = 0;
    grid->cols = 0;
    grid->data = NULL;

    char* line = NULL;
    size_t len = 0;
    ssize_t read;
    int max_cols = 0;

    // First pass to determine dimensions
    while ((read = getline(&line, &len, fp)) != -1) {
        if (read > 0 && line[read - 1] == '\n') {
            line[read - 1] = '\0';
            read--;
        }
         if (read > 0) {
            grid->rows++;
            if (read > max_cols) {
                max_cols = read;
            }
        }
    }
    grid->cols = max_cols;

    if (grid->rows == 0 || grid->cols == 0) {
        fprintf(stderr, "Input file is empty or invalid.\n");
        fclose(fp);
        free(line);
        free(grid);
        return NULL;
    }

    // Allocate grid data
    grid->data = (char**)malloc(grid->rows * sizeof(char*));
     if (!grid->data) {
         perror("Failed to allocate grid data rows");
         fclose(fp);
         free(line);
         free(grid);
         return NULL;
     }
    for (int i = 0; i < grid->rows; ++i) {
        grid->data[i] = (char*)malloc((grid->cols + 1) * sizeof(char)); // +1 for null terminator
         if (!grid->data[i]) {
             perror("Failed to allocate grid data cols");
             for (int k = 0; k < i; ++k) free(grid->data[k]);
             free(grid->data);
             fclose(fp);
             free(line);
             free(grid);
             return NULL;
         }
    }


    // Second pass to fill the grid
    rewind(fp);
    int current_row = 0;
    while ((read = getline(&line, &len, fp)) != -1 && current_row < grid->rows) {
         if (read > 0 && line[read - 1] == '\n') {
            line[read - 1] = '\0';
            read--;
        }
        if(read > 0) {
             strncpy(grid->data[current_row], line, grid->cols);
             grid->data[current_row][grid->cols] = '\0'; // Ensure null termination
             current_row++;
        }
    }

    free(line);
    fclose(fp);
    return grid;
}

void free_grid(Grid* grid) {
    if (!grid) return;
    if (grid->data) {
        for (int i = 0; i < grid->rows; ++i) {
            free(grid->data[i]);
        }
        free(grid->data);
    }
    free(grid);
}

void find_pois(Grid* grid, int poi_coords[][2], int* num_pois) {
    *num_pois = 0;
    int temp_coords[MAX_POIS][2];
    int max_poi_found = -1;

    for (int r = 0; r < grid->rows; ++r) {
        for (int c = 0; c < grid->cols; ++c) {
            if (isdigit(grid->data[r][c])) {
                int poi_index = grid->data[r][c] - '0';
                if (poi_index >= 0 && poi_index < MAX_POIS) {
                    temp_coords[poi_index][0] = r;
                    temp_coords[poi_index][1] = c;
                    if (poi_index > max_poi_found) {
                        max_poi_found = poi_index;
                    }
                }
            }
        }
    }
     *num_pois = max_poi_found + 1;
     // Copy valid coordinates to the output array
     memcpy(poi_coords, temp_coords, (*num_pois) * 2 * sizeof(int));
}


// --- BFS Implementation ---

int* bfs_get_edge_weights(Grid* grid, int start_row, int start_col, int num_pois) {
    int* distances = (int*)malloc(num_pois * sizeof(int));
    if (!distances) {
        perror("Failed to allocate distances array");
        return NULL;
    }
    for (int i = 0; i < num_pois; ++i) {
        distances[i] = INT_MAX; // Initialize distances as unreachable
    }

    // Allocate visited array for BFS
    int** visited = (int**)malloc(grid->rows * sizeof(int*));
    if (!visited) {
        perror("Failed to allocate visited rows");
        free(distances);
        return NULL;
    }
    for (int i = 0; i < grid->rows; i++) {
        visited[i] = (int*)calloc(grid->cols, sizeof(int)); // Use calloc for zero initialization
         if (!visited[i]) {
            perror("Failed to allocate visited columns");
            for(int k = 0; k < i; ++k) free(visited[k]);
            free(visited);
            free(distances);
            return NULL;
         }
    }

    Queue* queue = create_queue(grid->rows * grid->cols);
     if (!queue) {
        for(int k = 0; k < grid->rows; ++k) free(visited[k]);
        free(visited);
        free(distances);
        return NULL;
     }

    int start_poi_index = grid->data[start_row][start_col] - '0';
    distances[start_poi_index] = 0;
    visited[start_row][start_col] = 1;
    enqueue(queue, (BfsNode){start_row, start_col, 0});

    int dr[] = {-1, 1, 0, 0};
    int dc[] = {0, 0, -1, 1};

    while (!is_queue_empty(queue)) {
        BfsNode current = dequeue(queue);

        // Check if current location is a POI
        if (isdigit(grid->data[current.row][current.col])) {
            int poi_index = grid->data[current.row][current.col] - '0';
            // Update distance if this path is shorter (BFS guarantees this is the first time for shortest)
             if (distances[poi_index] == INT_MAX) {
                 distances[poi_index] = current.distance;
             }
        }

        // Explore neighbors
        for (int i = 0; i < 4; ++i) {
            int next_row = current.row + dr[i];
            int next_col = current.col + dc[i];

            // Check bounds and walls
            if (next_row >= 0 && next_row < grid->rows &&
                next_col >= 0 && next_col < grid->cols &&
                grid->data[next_row][next_col] != '#' &&
                !visited[next_row][next_col])
            {
                visited[next_row][next_col] = 1;
                enqueue(queue, (BfsNode){next_row, next_col, current.distance + 1});
            }
        }
    }

    // Free BFS resources
    free_queue(queue);
     for (int i = 0; i < grid->rows; ++i) {
        free(visited[i]);
    }
    free(visited);

    return distances;
}

// --- DFS (TSP Solver) ---

int dfs(int** graph, int num_pois, int current_poi, int visited_mask) {
    // Base case: all POIs visited
    if (visited_mask == (1 << num_pois) - 1) {
        return 0; // No need to return to 0
    }

    int min_dist = INT_MAX;

    for (int next_poi = 0; next_poi < num_pois; ++next_poi) {
        // If next_poi is not visited yet
        if (!(visited_mask & (1 << next_poi))) {
            int dist_to_next = graph[current_poi][next_poi];

            // Check if reachable
            if (dist_to_next != INT_MAX) {
                int remaining_dist = dfs(graph, num_pois, next_poi, visited_mask | (1 << next_poi));

                // If a valid path was found from the next_poi
                if (remaining_dist != INT_MAX) {
                    int total_dist = dist_to_next + remaining_dist;
                    if (total_dist < min_dist) {
                        min_dist = total_dist;
                    }
                }
            }
        }
    }

    return min_dist;
}

// --- Queue Implementation (Circular Array) ---

Queue* create_queue(int capacity) {
    Queue* q = (Queue*)malloc(sizeof(Queue));
    if (!q) {
         perror("Failed to allocate queue struct");
         return NULL;
     }
    q->nodes = (BfsNode*)malloc(capacity * sizeof(BfsNode));
     if (!q->nodes) {
         perror("Failed to allocate queue nodes");
         free(q);
         return NULL;
     }
    q->capacity = capacity;
    q->size = 0;
    q->head = 0;
    q->tail = 0;
    return q;
}

void free_queue(Queue* q) {
    if (!q) return;
    free(q->nodes);
    free(q);
}

int is_queue_empty(Queue* q) {
    return q->size == 0;
}

void enqueue(Queue* q, BfsNode node) {
    if (q->size == q->capacity) {
        // This should not happen if capacity is rows * cols
        fprintf(stderr, "Queue overflow\n");
        // In a real scenario, might reallocate or handle error
        return;
    }
    q->nodes[q->tail] = node;
    q->tail = (q->tail + 1) % q->capacity;
    q->size++;
}

BfsNode dequeue(Queue* q) {
    if (is_queue_empty(q)) {
        // This should not happen in the BFS logic if checked properly
        fprintf(stderr, "Queue underflow\n");
        return (BfsNode){-1, -1, -1}; // Return an invalid node
    }
    BfsNode node = q->nodes[q->head];
    q->head = (q->head + 1) % q->capacity;
    q->size--;
    return node;
}
