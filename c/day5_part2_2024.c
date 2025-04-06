
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LEN 1024
#define INITIAL_CAPACITY 16

// --- Dynamic Array for ints ---
typedef struct {
    int *data;
    int size;
    int capacity;
} IntArray;

void init_int_array(IntArray *arr) {
    arr->data = malloc(INITIAL_CAPACITY * sizeof(int));
    if (!arr->data) { perror("malloc failed"); exit(EXIT_FAILURE); }
    arr->size = 0;
    arr->capacity = INITIAL_CAPACITY;
}

void add_int(IntArray *arr, int value) {
    if (arr->size >= arr->capacity) {
        arr->capacity *= 2;
        arr->data = realloc(arr->data, arr->capacity * sizeof(int));
        if (!arr->data) { perror("realloc failed"); exit(EXIT_FAILURE); }
    }
    arr->data[arr->size++] = value;
}

void free_int_array(IntArray *arr) {
    free(arr->data);
    arr->data = NULL;
    arr->size = 0;
    arr->capacity = 0;
}

// --- Rule Structure ---
typedef struct {
    int x;
    int y;
} Rule;

// --- Update Structure ---
typedef struct {
    IntArray pages;
} Update;

// --- Simple Hash Map (int -> int) for positions and in-degrees ---
// Using open addressing with linear probing for simplicity
typedef struct {
    int key;
    int value;
    bool occupied;
} HashEntry;

typedef struct {
    HashEntry *entries;
    int capacity;
    int size;
} HashMap;

// Simple hash function
unsigned int hash_func(int key, int capacity) {
    // Basic modulo hashing, ensure non-negative result
    unsigned int hash = (unsigned int)key;
    return hash % capacity;
}

void init_hash_map(HashMap *map, int initial_capacity) {
    map->capacity = initial_capacity > 0 ? initial_capacity : INITIAL_CAPACITY;
    map->entries = calloc(map->capacity, sizeof(HashEntry));
     if (!map->entries) { perror("calloc failed"); exit(EXIT_FAILURE); }
    map->size = 0;
}

// Returns pointer to value if key exists, NULL otherwise
int* hash_map_get(HashMap *map, int key) {
    unsigned int index = hash_func(key, map->capacity);
    unsigned int start_index = index;
    while (map->entries[index].occupied) {
        if (map->entries[index].key == key) {
            return &map->entries[index].value;
        }
        index = (index + 1) % map->capacity;
        if (index == start_index) break; // Full circle
    }
    return NULL;
}

// Puts key-value pair. Assumes map won't get too full (no resize implemented)
void hash_map_put(HashMap *map, int key, int value) {
     if (map->size * 2 >= map->capacity) {
         // Basic resize needed for robustness, but omitted for brevity/simplicity here
         // fprintf(stderr, "Warning: Hash map potentially too full, performance may degrade.\n");
     }

    unsigned int index = hash_func(key, map->capacity);
    unsigned int start_index = index;
    while (map->entries[index].occupied) {
        if (map->entries[index].key == key) {
            map->entries[index].value = value; // Update existing key
            return;
        }
        index = (index + 1) % map->capacity;
         if (index == start_index) { // Should not happen if resize is implemented
             fprintf(stderr, "Error: Hash map full and key not found during put.\n");
             exit(EXIT_FAILURE);
         }
    }
    // Found empty slot
    map->entries[index].key = key;
    map->entries[index].value = value;
    map->entries[index].occupied = true;
    map->size++;
}

void free_hash_map(HashMap *map) {
    free(map->entries);
    map->entries = NULL;
    map->capacity = 0;
    map->size = 0;
}

// --- Simple Hash Map (int -> IntArray*) for adjacency list ---
typedef struct {
    int key;
    IntArray* value; // Pointer to a dynamic array
    bool occupied;
} HashAdjEntry;

typedef struct {
    HashAdjEntry *entries;
    int capacity;
    int size;
} HashMapAdj;


void init_hash_map_adj(HashMapAdj *map, int initial_capacity) {
    map->capacity = initial_capacity > 0 ? initial_capacity : INITIAL_CAPACITY;
    map->entries = calloc(map->capacity, sizeof(HashAdjEntry));
     if (!map->entries) { perror("calloc failed"); exit(EXIT_FAILURE); }
    map->size = 0;
}

IntArray* hash_map_adj_get(HashMapAdj *map, int key) {
    unsigned int index = hash_func(key, map->capacity);
    unsigned int start_index = index;
    while (map->entries[index].occupied) {
        if (map->entries[index].key == key) {
            return map->entries[index].value;
        }
        index = (index + 1) % map->capacity;
        if (index == start_index) break;
    }
    return NULL;
}

// Gets or creates the IntArray for the key
IntArray* hash_map_adj_get_or_create(HashMapAdj *map, int key) {
    IntArray* existing = hash_map_adj_get(map, key);
    if (existing) {
        return existing;
    }

     if (map->size * 2 >= map->capacity) {
         // Resize needed
         // fprintf(stderr, "Warning: Adj Hash map potentially too full.\n");
     }

    // Key not found, create and insert
    unsigned int index = hash_func(key, map->capacity);
    unsigned int start_index = index;
     while (map->entries[index].occupied) {
        // Should not find key here based on previous get, just find empty slot
        index = (index + 1) % map->capacity;
         if (index == start_index) {
             fprintf(stderr, "Error: Adj Hash map full during get_or_create.\n");
             exit(EXIT_FAILURE);
         }
    }

    map->entries[index].key = key;
    map->entries[index].value = malloc(sizeof(IntArray));
    if (!map->entries[index].value) { perror("malloc failed"); exit(EXIT_FAILURE); }
    init_int_array(map->entries[index].value);
    map->entries[index].occupied = true;
    map->size++;
    return map->entries[index].value;
}


void free_hash_map_adj(HashMapAdj *map) {
     for (int i = 0; i < map->capacity; ++i) {
        if (map->entries[i].occupied && map->entries[i].value) {
            free_int_array(map->entries[i].value);
            free(map->entries[i].value);
        }
    }
    free(map->entries);
    map->entries = NULL;
    map->capacity = 0;
    map->size = 0;
}


// --- Queue for Topological Sort ---
typedef struct {
    int *data;
    int capacity;
    int head;
    int tail;
    int size;
} Queue;

void init_queue(Queue *q, int initial_capacity) {
    q->capacity = initial_capacity > 0 ? initial_capacity : INITIAL_CAPACITY;
    q->data = malloc(q->capacity * sizeof(int));
    if (!q->data) { perror("malloc failed"); exit(EXIT_FAILURE); }
    q->head = 0;
    q->tail = 0;
    q->size = 0;
}

bool is_queue_empty(Queue *q) {
    return q->size == 0;
}

void enqueue(Queue *q, int value) {
    if (q->size >= q->capacity) {
        int new_capacity = q->capacity * 2;
        int *new_data = malloc(new_capacity * sizeof(int));
        if (!new_data) { perror("malloc failed"); exit(EXIT_FAILURE); }
        // Copy elements carefully considering wrap-around
        for (int i = 0; i < q->size; ++i) {
            new_data[i] = q->data[(q->head + i) % q->capacity];
        }
        free(q->data);
        q->data = new_data;
        q->head = 0;
        q->tail = q->size;
        q->capacity = new_capacity;
    }
    q->data[q->tail] = value;
    q->tail = (q->tail + 1) % q->capacity;
    q->size++;
}

int dequeue(Queue *q) {
    if (is_queue_empty(q)) {
        fprintf(stderr, "Dequeue from empty queue\n");
        exit(EXIT_FAILURE);
    }
    int value = q->data[q->head];
    q->head = (q->head + 1) % q->capacity;
    q->size--;
    return value;
}

void free_queue(Queue *q) {
    free(q->data);
    q->data = NULL;
    q->capacity = 0;
    q->head = 0;
    q->tail = 0;
    q->size = 0;
}


// --- Core Logic Functions ---

bool is_correct(Update *update, Rule *rules, int rule_count) {
    HashMap pos_map;
    init_hash_map(&pos_map, update->pages.size * 2); // Estimate capacity

    for (int i = 0; i < update->pages.size; ++i) {
        hash_map_put(&pos_map, update->pages.data[i], i);
    }

    bool correct = true;
    for (int i = 0; i < rule_count; ++i) {
        int x = rules[i].x;
        int y = rules[i].y;
        int *pos_x_ptr = hash_map_get(&pos_map, x);
        int *pos_y_ptr = hash_map_get(&pos_map, y);

        if (pos_x_ptr && pos_y_ptr) { // Both pages are in the update
            if (*pos_x_ptr > *pos_y_ptr) {
                correct = false;
                break;
            }
        }
    }

    free_hash_map(&pos_map);
    return correct;
}

IntArray correct_order(Update *update, Rule *rules, int rule_count) {
    int n = update->pages.size;
    HashMap indeg_map;
    HashMapAdj adj_map;
    init_hash_map(&indeg_map, n * 2);
    init_hash_map_adj(&adj_map, n * 2);

    // Initialize indegrees and adjacency lists for pages in the update
    for (int i = 0; i < n; ++i) {
        int page = update->pages.data[i];
        hash_map_put(&indeg_map, page, 0);
        hash_map_adj_get_or_create(&adj_map, page); // Ensure entry exists
    }

    // Build graph based on relevant rules
    for (int i = 0; i < rule_count; ++i) {
        int x = rules[i].x;
        int y = rules[i].y;

        // Check if both x and y are in the current update (exist in indeg_map)
        int *indeg_x_ptr = hash_map_get(&indeg_map, x);
        int *indeg_y_ptr = hash_map_get(&indeg_map, y);

        if (indeg_x_ptr && indeg_y_ptr) {
            // Add edge x -> y
            IntArray* neighbours_of_x = hash_map_adj_get(&adj_map, x); // Should exist
            add_int(neighbours_of_x, y);
            // Increment in-degree of y
            hash_map_put(&indeg_map, y, *indeg_y_ptr + 1);
        }
    }

    // Topological sort (Kahn's algorithm)
    Queue q;
    init_queue(&q, n);
    IntArray sorted_output;
    init_int_array(&sorted_output);

    // Add initial nodes with in-degree 0 to the queue
    for (int i = 0; i < n; ++i) {
        int page = update->pages.data[i];
        int *indeg_ptr = hash_map_get(&indeg_map, page);
        if (indeg_ptr && *indeg_ptr == 0) {
            enqueue(&q, page);
        }
    }

    while (!is_queue_empty(&q)) {
        int u = dequeue(&q);
        add_int(&sorted_output, u);

        IntArray* neighbours = hash_map_adj_get(&adj_map, u);
        if (neighbours) {
            for (int i = 0; i < neighbours->size; ++i) {
                int v = neighbours->data[i];
                int *indeg_v_ptr = hash_map_get(&indeg_map, v);
                 if (indeg_v_ptr) { // Should always be true if graph built correctly
                    int new_indeg = *indeg_v_ptr - 1;
                    hash_map_put(&indeg_map, v, new_indeg);
                    if (new_indeg == 0) {
                        enqueue(&q, v);
                    }
                }
            }
        }
    }

    // Cleanup
    free_queue(&q);
    free_hash_map_adj(&adj_map); // Frees internal IntArrays too
    free_hash_map(&indeg_map);

    // Check for cycles (optional, but good practice)
    if (sorted_output.size != n) {
         fprintf(stderr, "Warning: Cycle detected or error in topological sort for an update.\n");
         // Depending on requirements, might return empty/error or partial result.
         // For this problem, the Python code assumes no cycles in relevant subgraphs.
    }


    return sorted_output; // Caller must free this array's data
}

// --- Main Function ---
int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        return EXIT_FAILURE;
    }

    IntArray rules_x, rules_y;
    IntArray updates_storage; // Stores all update pages contiguously
    IntArray update_indices; // Stores start index of each update in updates_storage
    IntArray update_counts;  // Stores count of pages for each update

    init_int_array(&rules_x);
    init_int_array(&rules_y);
    init_int_array(&updates_storage);
    init_int_array(&update_indices);
    init_int_array(&update_counts);

    char line[MAX_LINE_LEN];
    bool reading_rules = true;

    add_int(&update_indices, 0); // First update starts at index 0

    while (fgets(line, sizeof(line), fp)) {
        // Trim trailing newline/whitespace
        line[strcspn(line, "\r\n")] = 0;
        if (strlen(line) == 0) continue; // Skip empty lines

        if (strchr(line, '|')) {
            if (!reading_rules) {
                 // This assumes rules always come before updates, matching Python logic.
                 // If mixed input is possible, this needs adjustment.
                 reading_rules = true; // Or handle error
            }
            int x, y;
            if (sscanf(line, "%d|%d", &x, &y) == 2) {
                 add_int(&rules_x, x);
                 add_int(&rules_y, y);
            } else {
                 fprintf(stderr, "Warning: Malformed rule line: %s\n", line);
            }
        } else {
            reading_rules = false; // Once we see a non-rule line, assume updates start
            int count = 0;
            char *token = strtok(line, ",");
            while (token != NULL) {
                add_int(&updates_storage, atoi(token));
                count++;
                token = strtok(NULL, ",");
            }
            if (count > 0) {
                add_int(&update_counts, count);
                add_int(&update_indices, updates_storage.size); // Next update starts here
            }
        }
    }
    fclose(fp);

    // Combine rules_x and rules_y into Rule array for easier passing
    int rule_count = rules_x.size;
    Rule *rules = malloc(rule_count * sizeof(Rule));
     if (!rules) { perror("malloc failed"); exit(EXIT_FAILURE); }
    for(int i=0; i<rule_count; ++i) {
        rules[i].x = rules_x.data[i];
        rules[i].y = rules_y.data[i];
    }
    free_int_array(&rules_x);
    free_int_array(&rules_y);


    long long total_sum_incorrect_middle = 0;
    int num_updates = update_counts.size;

    for (int i = 0; i < num_updates; ++i) {
        Update current_update;
        current_update.pages.data = updates_storage.data + update_indices.data[i];
        current_update.pages.size = update_counts.data[i];
        current_update.pages.capacity = current_update.pages.size; // Not strictly needed, just for struct consistency

        if (!is_correct(&current_update, rules, rule_count)) {
            IntArray fixed_order = correct_order(&current_update, rules, rule_count);
            if (fixed_order.size > 0) {
                 // Check if fixed_order.size matches current_update.pages.size if needed
                 total_sum_incorrect_middle += fixed_order.data[fixed_order.size / 2];
            }
             // IMPORTANT: Free the data allocated by correct_order
             free_int_array(&fixed_order);
        }
    }

    printf("%lld\n", total_sum_incorrect_middle);

    // Final cleanup
    free(rules);
    free_int_array(&updates_storage);
    free_int_array(&update_indices);
    free_int_array(&update_counts);

    return EXIT_SUCCESS;
}
