
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// --- Point Struct ---
typedef struct {
    int x;
    int y;
} Point;

// --- Hash Map Implementation (Separate Chaining) ---
typedef struct MapNode {
    Point key;
    int value;
    struct MapNode *next;
} MapNode;

typedef struct {
    MapNode **table;
    size_t capacity;
    size_t size;
} Map;

// Simple hash function for Point
unsigned int hash_point(Point p, size_t capacity) {
    unsigned int hash = (unsigned int)p.x * 31u + (unsigned int)p.y;
    return hash % capacity;
}

Map *map_create(size_t initial_capacity) {
    Map *map = malloc(sizeof(Map));
    if (!map) return NULL;
    map->capacity = initial_capacity;
    map->size = 0;
    map->table = calloc(map->capacity, sizeof(MapNode *));
    if (!map->table) {
        free(map);
        return NULL;
    }
    return map;
}

void map_destroy(Map *map) {
    if (!map) return;
    for (size_t i = 0; i < map->capacity; ++i) {
        MapNode *current = map->table[i];
        while (current != NULL) {
            MapNode *next = current->next;
            free(current);
            current = next;
        }
    }
    free(map->table);
    free(map);
}

// Gets the value associated with the key, returns -1 if not found
int map_get(Map *map, Point key) {
    unsigned int index = hash_point(key, map->capacity);
    MapNode *node = map->table[index];
    while (node != NULL) {
        if (node->key.x == key.x && node->key.y == key.y) {
            return node->value;
        }
        node = node->next;
    }
    return -1; // Indicate not found
}

// Updates value if key exists and new_value is smaller, or inserts if key doesn't exist.
// Returns true if inserted or updated, false otherwise.
bool map_update_min(Map *map, Point key, int value) {
     // Basic resize check (can be made more sophisticated)
    if (map->size >= map->capacity * 0.75) {
        // In a real scenario, implement resizing here.
        // For this problem, assume initial capacity is sufficient or increase it.
        // Resizing involves creating a new larger table, rehashing all existing
        // elements, and freeing the old table. We'll skip it for brevity
        // assuming the initial guess is large enough. If coordinates get very
        // large or sparse, collisions might degrade performance without resizing.
    }

    unsigned int index = hash_point(key, map->capacity);
    MapNode *node = map->table[index];
    MapNode *prev = NULL;

    while (node != NULL) {
        if (node->key.x == key.x && node->key.y == key.y) {
            if (value < node->value) {
                node->value = value;
                return true; // Updated
            }
            return false; // Not updated (existing value is smaller or equal)
        }
        prev = node;
        node = node->next;
    }

    // Key not found, insert new node
    MapNode *new_node = malloc(sizeof(MapNode));
    if (!new_node) return false; // Allocation failed
    new_node->key = key;
    new_node->value = value;
    new_node->next = map->table[index]; // Insert at head
    map->table[index] = new_node;
    map->size++;
    return true; // Inserted
}

// --- Stack Implementation (Dynamic Array) ---
typedef struct {
    Point *items;
    size_t capacity;
    size_t top; // Index of the next free spot (also current size)
} Stack;

Stack *stack_create(size_t initial_capacity) {
    Stack *stack = malloc(sizeof(Stack));
    if (!stack) return NULL;
    stack->capacity = initial_capacity;
    stack->top = 0;
    stack->items = malloc(stack->capacity * sizeof(Point));
    if (!stack->items) {
        free(stack);
        return NULL;
    }
    return stack;
}

void stack_destroy(Stack *stack) {
    if (!stack) return;
    free(stack->items);
    free(stack);
}

bool stack_push(Stack *stack, Point item) {
    if (stack->top >= stack->capacity) {
        size_t new_capacity = stack->capacity == 0 ? 16 : stack->capacity * 2;
        Point *new_items = realloc(stack->items, new_capacity * sizeof(Point));
        if (!new_items) return false; // Reallocation failed
        stack->items = new_items;
        stack->capacity = new_capacity;
    }
    stack->items[stack->top++] = item;
    return true;
}

// Returns the item from the top without removing it
Point stack_peek(Stack *stack) {
    // Assumes stack is not empty, add checks if necessary
    return stack->items[stack->top - 1];
}

// Removes and returns the item from the top
Point stack_pop(Stack *stack) {
    // Assumes stack is not empty, add checks if necessary
    return stack->items[--stack->top];
}

// --- Main Logic ---
int main() {
    FILE *f = fopen("input.txt", "r");
    if (!f) {
        perror("Error opening input.txt");
        return 1;
    }

    // Find file size
    fseek(f, 0, SEEK_END);
    long file_size = ftell(f);
    if (file_size == -1) {
         perror("Error getting file size");
         fclose(f);
         return 1;
    }
    rewind(f);

    // Read file content
    char *directions = malloc(file_size + 1);
    if (!directions) {
        fprintf(stderr, "Error allocating memory for directions\n");
        fclose(f);
        return 1;
    }
    size_t bytes_read = fread(directions, 1, file_size, f);
     if (bytes_read < file_size && ferror(f)) {
        perror("Error reading file");
        free(directions);
        fclose(f);
        return 1;
    }
    directions[bytes_read] = '\0'; // Null-terminate
    fclose(f);

    // Find the actual end, stripping potential whitespace like Python's strip()
    // We are primarily interested in the last relevant char before any trailing whitespace.
    // The python code uses directions[1:-1], so we need start and end pointers/indices.
    char *start = directions;
    char *end = directions + bytes_read -1; // Point to last char read

    // Find first char (should be '^')
     while (*start && *start != '^') start++;
     if (*start == '^') start++; // Move past '^'

    // Find last char (should be '$') ignoring trailing whitespace
    while (end > start && (*end == '$' || *end == '\n' || *end == '\r' || *end == ' ')) end--;
    // end now points to the last char before '$' or whitespace

    // Initialize data structures
    // Increased initial capacity for potentially large maps/stacks
    Map *rooms = map_create(16384);
    Stack *stack = stack_create(1024);
    if (!rooms || !stack) {
        fprintf(stderr, "Error initializing map or stack\n");
        map_destroy(rooms);
        stack_destroy(stack);
        free(directions);
        return 1;
    }

    Point current_pos = {0, 0};
    map_update_min(rooms, current_pos, 0);

    // Process directions string
    for (char *c = start; c <= end; ++c) {
        switch (*c) {
            case '(':
                if (!stack_push(stack, current_pos)) {
                    fprintf(stderr, "Error pushing to stack\n");
                    goto cleanup; // Use goto for cleaner multi-level exit
                }
                break;
            case '|':
                current_pos = stack_peek(stack);
                break;
            case ')':
                current_pos = stack_pop(stack);
                break;
            case 'N':
            case 'E':
            case 'S':
            case 'W': {
                int dx = 0, dy = 0;
                if (*c == 'N') dy = -1;
                else if (*c == 'E') dx = 1;
                else if (*c == 'S') dy = 1;
                else if (*c == 'W') dx = -1;

                Point next_pos = {current_pos.x + dx, current_pos.y + dy};
                int current_doors = map_get(rooms, current_pos);
                // current_doors should always be found if logic is correct
                 if (current_doors == -1) {
                     fprintf(stderr, "Error: current position (%d, %d) not found in map!\n", current_pos.x, current_pos.y);
                      goto cleanup;
                 }
                int next_doors = current_doors + 1;
                map_update_min(rooms, next_pos, next_doors);
                current_pos = next_pos;
                break;
            }
             default:
                // Ignore unexpected characters if any
                break;
        }
    }

    // Calculate results
    int max_doors = 0;
    int rooms_with_1000_doors = 0;
    for (size_t i = 0; i < rooms->capacity; ++i) {
        MapNode *node = rooms->table[i];
        while (node != NULL) {
            if (node->value > max_doors) {
                max_doors = node->value;
            }
            if (node->value >= 1000) {
                rooms_with_1000_doors++;
            }
            node = node->next;
        }
    }

    // Print results
    printf("%d\n", max_doors);
    printf("%d\n", rooms_with_1000_doors);

cleanup:
    // Clean up resources
    map_destroy(rooms);
    stack_destroy(stack);
    free(directions);

    return 0;
}
