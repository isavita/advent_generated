
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <limits.h>

#define MAX_DIM 100
#define MAX_KEYS 26
#define NUM_ROBOTS 4
#define MAX_NODES (MAX_KEYS + NUM_ROBOTS) // Max keys + 4 robots
#define HASH_TABLE_SIZE 16777213 // A prime number for better distribution

// Map and dimensions
char original_map[MAX_DIM][MAX_DIM];
int width = 0, height = 0;

// Key and robot info
typedef struct { int x, y; } Point;
Point key_positions[MAX_KEYS];
Point robot_start_positions[NUM_ROBOTS];
int num_keys = 0;
int all_keys_mask = 0;

// Node IDs: 0 to num_keys-1 for keys 'a' to 'z', num_keys to num_keys+3 for robots 0 to 3
Point node_coords[MAX_NODES];
int node_count = 0;

// Graph representation (Adjacency List)
typedef struct {
    int to_node;
    int dist;
    int required_keys_mask;
} Edge;

typedef struct {
    Edge *edges;
    int count;
    int capacity;
} NodeEdges;

NodeEdges key_graph[MAX_NODES];

// --- BFS for building key_graph ---
typedef struct {
    int x, y, dist, required_keys_mask;
} BFSState;

typedef struct {
    BFSState items[MAX_DIM * MAX_DIM];
    int head;
    int tail;
    int size;
} Queue;

void queue_init(Queue* q) {
    q->head = 0;
    q->tail = 0;
    q->size = 0;
}

void queue_push(Queue* q, BFSState item) {
    q->items[q->tail] = item;
    q->tail = (q->tail + 1) % (MAX_DIM * MAX_DIM);
    q->size++;
}

BFSState queue_pop(Queue* q) {
    BFSState item = q->items[q->head];
    q->head = (q->head + 1) % (MAX_DIM * MAX_DIM);
    q->size--;
    return item;
}

bool queue_is_empty(Queue* q) {
    return q->size == 0;
}

// --- Dijkstra's Algorithm ---
typedef struct {
    int cost;
    int robot_positions[NUM_ROBOTS]; // Node IDs
    int collected_keys_mask;
} DijkstraState;

// Min-Heap for Dijkstra
typedef struct {
    DijkstraState *states;
    int size;
    int capacity;
} MinHeap;

void heap_init(MinHeap* h, int capacity) {
    h->states = (DijkstraState*)malloc(capacity * sizeof(DijkstraState));
    h->size = 0;
    h->capacity = capacity;
}

void swap_states(DijkstraState* a, DijkstraState* b) {
    DijkstraState temp = *a;
    *a = *b;
    *b = temp;
}

void heap_push(MinHeap* h, DijkstraState state) {
    if (h->size == h->capacity) {
        h->capacity *= 2;
        h->states = (DijkstraState*)realloc(h->states, h->capacity * sizeof(DijkstraState));
    }
    h->states[h->size] = state;
    int current = h->size++;
    while (current > 0) {
        int parent = (current - 1) / 2;
        if (h->states[current].cost < h->states[parent].cost) {
            swap_states(&h->states[current], &h->states[parent]);
            current = parent;
        } else {
            break;
        }
    }
}

DijkstraState heap_pop(MinHeap* h) {
    DijkstraState min_state = h->states[0];
    h->states[0] = h->states[--h->size];
    int current = 0;
    while (true) {
        int left = 2 * current + 1;
        int right = 2 * current + 2;
        int smallest = current;
        if (left < h->size && h->states[left].cost < h->states[smallest].cost) {
            smallest = left;
        }
        if (right < h->size && h->states[right].cost < h->states[smallest].cost) {
            smallest = right;
        }
        if (smallest != current) {
            swap_states(&h->states[current], &h->states[smallest]);
            current = smallest;
        } else {
            break;
        }
    }
    return min_state;
}

bool heap_is_empty(MinHeap* h) {
    return h->size == 0;
}

void heap_free(MinHeap* h) {
    free(h->states);
    h->states = NULL;
    h->size = 0;
    h->capacity = 0;
}

// --- Visited Set for Dijkstra (Hash Table) ---
typedef struct VisitedEntry {
    int robot_positions[NUM_ROBOTS];
    int collected_keys_mask;
    int cost;
    struct VisitedEntry* next;
} VisitedEntry;

VisitedEntry* visited_table[HASH_TABLE_SIZE];

unsigned int hash_state(int robot_positions[], int mask) {
    unsigned int hash = mask;
    // Simple hash combining mask and positions
    for (int i = 0; i < NUM_ROBOTS; ++i) {
        hash = (hash * 31) + robot_positions[i];
    }
    return hash % HASH_TABLE_SIZE;
}

// Returns current min cost if found, INT_MAX otherwise
int get_visited_cost(int robot_positions[], int mask) {
    unsigned int index = hash_state(robot_positions, mask);
    VisitedEntry* entry = visited_table[index];
    while (entry != NULL) {
        if (entry->collected_keys_mask == mask && memcmp(entry->robot_positions, robot_positions, NUM_ROBOTS * sizeof(int)) == 0) {
            return entry->cost;
        }
        entry = entry->next;
    }
    return INT_MAX;
}

// Adds or updates the cost in the visited set
void add_or_update_visited(int robot_positions[], int mask, int cost) {
    unsigned int index = hash_state(robot_positions, mask);
    VisitedEntry* entry = visited_table[index];
    VisitedEntry* prev = NULL;

    while (entry != NULL) {
        if (entry->collected_keys_mask == mask && memcmp(entry->robot_positions, robot_positions, NUM_ROBOTS * sizeof(int)) == 0) {
            entry->cost = cost; // Update cost
            return;
        }
        prev = entry;
        entry = entry->next;
    }

    // Not found, add new entry
    VisitedEntry* new_entry = (VisitedEntry*)malloc(sizeof(VisitedEntry));
    memcpy(new_entry->robot_positions, robot_positions, NUM_ROBOTS * sizeof(int));
    new_entry->collected_keys_mask = mask;
    new_entry->cost = cost;
    new_entry->next = NULL;

    if (prev == NULL) {
        visited_table[index] = new_entry; // First entry in this bucket
    } else {
        prev->next = new_entry;
    }
}

void free_visited_table() {
    for (int i = 0; i < HASH_TABLE_SIZE; ++i) {
        VisitedEntry* entry = visited_table[i];
        while (entry != NULL) {
            VisitedEntry* temp = entry;
            entry = entry->next;
            free(temp);
        }
        visited_table[i] = NULL;
    }
}

// --- Helper Functions ---
int get_key_id(char c) {
    return c - 'a';
}

int get_robot_id(int index) {
    return num_keys + index;
}

char get_node_char(int node_id) {
    if (node_id < num_keys) {
        return 'a' + node_id;
    } else {
        return '@'; // Represent robot starts generically
    }
}

void add_edge(int from_node, int to_node, int dist, int req_mask) {
    NodeEdges* node = &key_graph[from_node];
    if (node->count == node->capacity) {
        node->capacity = (node->capacity == 0) ? 4 : node->capacity * 2;
        node->edges = (Edge*)realloc(node->edges, node->capacity * sizeof(Edge));
    }
    node->edges[node->count++] = (Edge){to_node, dist, req_mask};
}

// --- Core Logic ---

// BFS from a single node (key or robot start) to find reachable keys
void run_key_bfs(int start_node_id) {
    Queue q;
    queue_init(&q);
    bool visited[MAX_DIM][MAX_DIM] = {false};

    Point start_coord = node_coords[start_node_id];
    queue_push(&q, (BFSState){start_coord.x, start_coord.y, 0, 0});
    visited[start_coord.y][start_coord.x] = true;

    while (!queue_is_empty(&q)) {
        BFSState current = queue_pop(&q);
        int x = current.x;
        int y = current.y;
        int dist = current.dist;
        int req_mask = current.required_keys_mask;

        char cell = original_map[y][x];

        // If we landed on a key (and it's not the start node if start was a key)
        if (islower(cell)) {
            int key_id = get_key_id(cell);
            if (key_id != start_node_id) { // Don't add edge to self
                 add_edge(start_node_id, key_id, dist, req_mask);
                 // Note: Python version implicitly stopped exploring beyond finding a key
                 // in the sense that it added the key to required_keys for further steps FROM that key.
                 // Here, we add the edge and continue BFS to find other keys from the same start.
                 // This matches the intent of finding all reachable keys from the start node.
            }
        }

        int dx[] = {-1, 1, 0, 0};
        int dy[] = {0, 0, -1, 1};

        for (int i = 0; i < 4; ++i) {
            int nx = x + dx[i];
            int ny = y + dy[i];

            if (nx >= 0 && nx < width && ny >= 0 && ny < height && !visited[ny][nx]) {
                char next_cell = original_map[ny][nx];
                if (next_cell != '#') {
                    visited[ny][nx] = true;
                    int next_req_mask = req_mask;
                    if (isupper(next_cell)) {
                        next_req_mask |= (1 << get_key_id(tolower(next_cell)));
                    }
                    queue_push(&q, (BFSState){nx, ny, dist + 1, next_req_mask});
                }
            }
        }
    }
}

int solve() {
    MinHeap pq;
    heap_init(&pq, 100000); // Initial capacity

    DijkstraState initial_state;
    initial_state.cost = 0;
    initial_state.collected_keys_mask = 0;
    for(int i = 0; i < NUM_ROBOTS; ++i) {
        initial_state.robot_positions[i] = get_robot_id(i); // Start at robot IDs
    }

    heap_push(&pq, initial_state);
    add_or_update_visited(initial_state.robot_positions, 0, 0);

    while (!heap_is_empty(&pq)) {
        DijkstraState current_state = heap_pop(&pq);
        int current_cost = current_state.cost;
        int current_mask = current_state.collected_keys_mask;

        // Optimization: check visited cost again after popping (lazy deletion)
        if (current_cost > get_visited_cost(current_state.robot_positions, current_mask)) {
            continue;
        }

        if (current_mask == all_keys_mask) {
            heap_free(&pq);
            return current_cost;
        }

        // Try moving each robot
        for (int i = 0; i < NUM_ROBOTS; ++i) {
            int robot_node_id = current_state.robot_positions[i];
            NodeEdges* edges = &key_graph[robot_node_id];

            for (int j = 0; j < edges->count; ++j) {
                Edge edge = edges->edges[j];
                int target_node_id = edge.to_node;
                int dist = edge.dist;
                int req_mask = edge.required_keys_mask;

                // Target must be a key
                if (target_node_id >= num_keys) continue; // Should not happen with current BFS logic, but safe check

                int target_key_bit = (1 << target_node_id);

                // Check if: 1. Key not already collected AND 2. Required keys for path are collected
                if (!(current_mask & target_key_bit) && ((req_mask & ~current_mask) == 0)) {
                    int new_mask = current_mask | target_key_bit;
                    int new_cost = current_cost + dist;

                    DijkstraState next_state = current_state; // Copy current state
                    next_state.cost = new_cost;
                    next_state.collected_keys_mask = new_mask;
                    next_state.robot_positions[i] = target_node_id; // Update position of moved robot

                    if (new_cost < get_visited_cost(next_state.robot_positions, new_mask)) {
                         add_or_update_visited(next_state.robot_positions, new_mask, new_cost);
                         heap_push(&pq, next_state);
                    }
                }
            }
        }
    }

    heap_free(&pq);
    return -1; // Should not happen if a solution exists
}


int main() {
    FILE *f = fopen("input.txt", "r");
    if (!f) {
        perror("Error opening input.txt");
        return 1;
    }

    char line[MAX_DIM];
    height = 0;
    while (fgets(line, sizeof(line), f) && height < MAX_DIM) {
        // Remove trailing newline
        line[strcspn(line, "\n")] = 0;
        if (strlen(line) == 0) continue;

        if (width == 0) {
            width = strlen(line);
        } else if (strlen(line) != width) {
             fprintf(stderr, "Error: Inconsistent map width.\n");
             fclose(f);
             return 1;
        }
        if (width >= MAX_DIM) {
            fprintf(stderr, "Error: Map width exceeds MAX_DIM.\n");
            fclose(f);
            return 1;
        }
        strcpy(original_map[height], line);
        height++;
    }
    fclose(f);

    // --- Part Two Modification ---
    int center_x = -1, center_y = -1;
    bool found_center = false;
    for (int y = 1; y < height - 1 && !found_center; ++y) {
        for (int x = 1; x < width - 1; ++x) {
            if (original_map[y][x] == '@') {
                 // Check surroundings (more robustly than python version perhaps)
                if (original_map[y-1][x] != '#' && original_map[y+1][x] != '#' &&
                    original_map[y][x-1] != '#' && original_map[y][x+1] != '#' &&
                    original_map[y-1][x-1] != '#' && original_map[y-1][x+1] != '#' &&
                    original_map[y+1][x-1] != '#' && original_map[y+1][x+1] != '#')
                {
                    center_x = x;
                    center_y = y;
                    found_center = true;
                    break;
                }
            }
        }
    }

    if (!found_center) {
        fprintf(stderr, "Error: Could not find the '@' symbol suitable for modification.\n");
        return 1;
    }

    // Apply modification
    original_map[center_y - 1][center_x - 1] = '@'; original_map[center_y - 1][center_x] = '#'; original_map[center_y - 1][center_x + 1] = '@';
    original_map[center_y    ][center_x - 1] = '#'; original_map[center_y    ][center_x] = '#'; original_map[center_y    ][center_x + 1] = '#';
    original_map[center_y + 1][center_x - 1] = '@'; original_map[center_y + 1][center_x] = '#'; original_map[center_y + 1][center_x + 1] = '@';

    // --- Find keys and robot starts ---
    num_keys = 0;
    int robot_idx = 0;
    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            char cell = original_map[y][x];
            if (islower(cell)) {
                int key_id = get_key_id(cell);
                key_positions[key_id] = (Point){x, y};
                node_coords[key_id] = (Point){x, y}; // Use key_id directly as node ID
                 if (key_id >= num_keys) num_keys = key_id + 1; // Track highest key ID found
            } else if (cell == '@') {
                 if (robot_idx < NUM_ROBOTS) {
                    robot_start_positions[robot_idx] = (Point){x, y};
                    // Assign node ID after all keys
                    node_coords[num_keys + robot_idx] = (Point){x, y};
                    robot_idx++;
                }
            }
        }
    }
     if (robot_idx != NUM_ROBOTS) {
        fprintf(stderr, "Error: Did not find exactly %d robots after modification.\n", NUM_ROBOTS);
        return 1;
     }

    node_count = num_keys + NUM_ROBOTS;
    all_keys_mask = (1 << num_keys) - 1;


    // --- Build Key Graph ---
    for (int i = 0; i < node_count; ++i) {
        key_graph[i].edges = NULL;
        key_graph[i].count = 0;
        key_graph[i].capacity = 0;
        run_key_bfs(i);
    }


    // --- Solve using Dijkstra ---
    for(int i=0; i < HASH_TABLE_SIZE; ++i) visited_table[i] = NULL; // Init hash table
    int result = solve();
    printf("%d\n", result);


    // --- Cleanup ---
    for (int i = 0; i < node_count; ++i) {
        free(key_graph[i].edges);
    }
    free_visited_table();


    return 0;
}
