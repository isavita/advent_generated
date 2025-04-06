
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <limits.h>

#define MAX_DIM 100
#define MAX_KEYS 26 // Max 'a' to 'z'
#define HASH_TABLE_SIZE (1 << 20) // Adjust size based on expected states

typedef struct {
    int x, y;
} Point;

typedef struct {
    Point pos;
    int keys; // Bitmask of collected keys
} State;

// Hash Node for separate chaining
typedef struct HashNode {
    State state;
    struct HashNode* next;
} HashNode;

// Hash Table
typedef struct {
    HashNode** buckets;
    int size;
} HashTable;

// Queue Node
typedef struct QueueNode {
    State state;
    struct QueueNode* next;
} QueueNode;

// Queue
typedef struct {
    QueueNode *front, *rear;
    int size;
} Queue;


// --- Hash Table Functions ---

unsigned int hash_state(State s, int table_size) {
    // Simple hash combining coordinates and keys
    unsigned int hash = 5381;
    hash = ((hash << 5) + hash) + s.pos.x;
    hash = ((hash << 5) + hash) + s.pos.y;
    hash = ((hash << 5) + hash) + s.keys;
    return hash % table_size;
}

HashTable* create_hash_table(int size) {
    HashTable* ht = (HashTable*)malloc(sizeof(HashTable));
    if (!ht) return NULL;
    ht->size = size;
    ht->buckets = (HashNode**)calloc(size, sizeof(HashNode*));
    if (!ht->buckets) {
        free(ht);
        return NULL;
    }
    return ht;
}

// Returns true if inserted, false if already exists
bool ht_insert(HashTable* ht, State s) {
    unsigned int index = hash_state(s, ht->size);
    HashNode* current = ht->buckets[index];
    while (current) {
        if (current->state.pos.x == s.pos.x &&
            current->state.pos.y == s.pos.y &&
            current->state.keys == s.keys) {
            return false; // Already visited
        }
        current = current->next;
    }

    // Insert new node
    HashNode* newNode = (HashNode*)malloc(sizeof(HashNode));
    if (!newNode) return false; // Allocation error, treat as visited to stop
    newNode->state = s;
    newNode->next = ht->buckets[index];
    ht->buckets[index] = newNode;
    return true;
}

void destroy_hash_table(HashTable* ht) {
    if (!ht) return;
    for (int i = 0; i < ht->size; ++i) {
        HashNode* current = ht->buckets[i];
        while (current) {
            HashNode* temp = current;
            current = current->next;
            free(temp);
        }
    }
    free(ht->buckets);
    free(ht);
}

// --- Queue Functions ---

Queue* create_queue() {
    Queue* q = (Queue*)malloc(sizeof(Queue));
     if (!q) return NULL;
    q->front = q->rear = NULL;
    q->size = 0;
    return q;
}

void enqueue(Queue* q, State s) {
    QueueNode* newNode = (QueueNode*)malloc(sizeof(QueueNode));
    if (!newNode) return; // Handle allocation failure
    newNode->state = s;
    newNode->next = NULL;
    if (q->rear == NULL) {
        q->front = q->rear = newNode;
    } else {
        q->rear->next = newNode;
        q->rear = newNode;
    }
    q->size++;
}

State dequeue(Queue* q) {
    State empty_state = {{-1, -1}, -1}; // Should not happen if called correctly
    if (q->front == NULL) return empty_state;
    QueueNode* temp = q->front;
    State s = temp->state;
    q->front = q->front->next;
    if (q->front == NULL) {
        q->rear = NULL;
    }
    free(temp);
    q->size--;
    return s;
}

bool is_empty(Queue* q) {
    return q->size == 0;
}

void destroy_queue(Queue* q) {
     if (!q) return;
     QueueNode *current = q->front;
     while(current != NULL) {
         QueueNode *temp = current;
         current = current->next;
         free(temp);
     }
     free(q);
}


// --- BFS Logic ---

int find_shortest_path(char grid[MAX_DIM][MAX_DIM], int height, int width, Point start, int key_map[MAX_KEYS], int num_keys) {
    Point dirs[] = {{0, -1}, {-1, 0}, {0, 1}, {1, 0}};
    int target_keys = (1 << num_keys) - 1;

    Queue* queue = create_queue();
    if (!queue) return -1;
    HashTable* visited = create_hash_table(HASH_TABLE_SIZE);
     if (!visited) {
        destroy_queue(queue);
        return -1;
    }

    State start_state = {start, 0};
    enqueue(queue, start_state);
    ht_insert(visited, start_state);

    int steps = 0;

    while (!is_empty(queue)) {
        int level_size = queue->size;
        for (int i = 0; i < level_size; ++i) {
            State current = dequeue(queue);

            if (current.keys == target_keys) {
                destroy_queue(queue);
                destroy_hash_table(visited);
                return steps;
            }

            for (int j = 0; j < 4; ++j) {
                Point next_pos = {current.pos.x + dirs[j].x, current.pos.y + dirs[j].y};

                if (next_pos.x >= 0 && next_pos.x < width && next_pos.y >= 0 && next_pos.y < height) {
                    char cell = grid[next_pos.y][next_pos.x];
                    int next_keys = current.keys;

                    if (cell == '#') {
                        continue; // Wall
                    }

                    if (isupper(cell)) {
                        int key_index = key_map[tolower(cell) - 'a'];
                        if (key_index == -1 || !(current.keys & (1 << key_index))) {
                            continue; // Locked door, no key
                        }
                    }

                    if (islower(cell)) {
                         int key_index = key_map[cell - 'a'];
                         if(key_index != -1) {
                             next_keys |= (1 << key_index);
                         }
                    }

                    State next_state = {next_pos, next_keys};
                    if (ht_insert(visited, next_state)) {
                        enqueue(queue, next_state);
                    }
                }
            }
        }
        steps++;
    }

    destroy_queue(queue);
    destroy_hash_table(visited);
    return -1; // No path found
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char grid[MAX_DIM][MAX_DIM];
    Point start = {-1, -1};
    int key_map[MAX_KEYS]; // Map 'a'->0, 'b'->1 etc. index
    for(int i=0; i<MAX_KEYS; ++i) key_map[i] = -1;
    int key_counter = 0;
    int height = 0;
    int width = 0;

    char line[MAX_DIM + 2]; // +1 for potential \n, +1 for \0
    while (height < MAX_DIM && fgets(line, sizeof(line), file)) {
        int len = strlen(line);
        // Remove trailing newline if present
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }
         if (len == 0) continue; // Skip empty lines

        if (width == 0) {
            width = len;
        } else if (len != width) {
             fprintf(stderr, "Error: Inconsistent line length %d (expected %d) at line %d\n", len, width, height + 1);
             fclose(file);
             return 1;
        }

        if (width >= MAX_DIM) {
             fprintf(stderr, "Error: Line width exceeds MAX_DIM\n");
             fclose(file);
             return 1;
        }

        strncpy(grid[height], line, width);
        grid[height][width] = '\0'; // Ensure null termination

        for (int x = 0; x < width; ++x) {
            if (line[x] == '@') {
                start.x = x;
                start.y = height;
                grid[height][x] = '.'; // Treat start as empty space
            } else if (islower(line[x])) {
                int key_index = line[x] - 'a';
                if (key_map[key_index] == -1) {
                    if(key_counter >= MAX_KEYS) {
                         fprintf(stderr, "Error: Exceeded maximum number of unique keys (%d)\n", MAX_KEYS);
                         fclose(file);
                         return 1;
                    }
                    key_map[key_index] = key_counter++;
                }
            }
        }
        height++;
    }
    fclose(file);

    if (start.x == -1) {
        fprintf(stderr, "Error: Start position '@' not found.\n");
        return 1;
    }
     if (key_counter == 0) {
         printf("0\n"); // No keys to collect
         return 0;
     }


    int result = find_shortest_path(grid, height, width, start, key_map, key_counter);
    printf("%d\n", result);

    return 0;
}
