
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_BLUEPRINTS 100
#define INITIAL_QUEUE_CAPACITY 1000000
#define HASH_TABLE_SIZE 19999999 // A large prime number

typedef struct {
    int id;
    int ore_cost;
    int clay_ore_cost;
    int obsidian_ore_cost;
    int obsidian_clay_cost;
    int geode_ore_cost;
    int geode_obsidian_cost;
    int max_ore_needed; // Precompute max ore needed for any robot
} Blueprint;

// Make state compact for memory efficiency
typedef struct {
    short ore;
    short clay;
    short obsidian;
    short ore_robots;
    short clay_robots;
    short obsidian_robots;
    short geode_robots;
    short time_left;
    int geode; // Geode can potentially exceed short limit
} State;

// --- Queue Implementation ---
typedef struct {
    State* data;
    size_t capacity;
    size_t size;
    size_t front;
    size_t rear;
} Queue;

Queue* create_queue(size_t capacity) {
    Queue* q = (Queue*)malloc(sizeof(Queue));
    q->data = (State*)malloc(capacity * sizeof(State));
    q->capacity = capacity;
    q->size = 0;
    q->front = 0;
    q->rear = 0;
    return q;
}

void resize_queue(Queue* q) {
    size_t new_capacity = q->capacity * 2;
    State* new_data = (State*)malloc(new_capacity * sizeof(State));
    if (!new_data) {
         perror("Failed to resize queue");
         exit(EXIT_FAILURE);
    }

    size_t j = 0;
    for (size_t i = 0; i < q->size; ++i) {
        new_data[j++] = q->data[(q->front + i) % q->capacity];
    }

    free(q->data);
    q->data = new_data;
    q->capacity = new_capacity;
    q->front = 0;
    q->rear = q->size;
}


void enqueue(Queue* q, State item) {
    if (q->size == q->capacity) {
       resize_queue(q);
    }
    q->data[q->rear] = item;
    q->rear = (q->rear + 1) % q->capacity;
    q->size++;
}

State dequeue(Queue* q) {
    State item = q->data[q->front];
    q->front = (q->front + 1) % q->capacity;
    q->size--;
    return item;
}

void destroy_queue(Queue* q) {
    free(q->data);
    free(q);
}

// --- Visited Set (Hash Table with Linear Probing) ---
typedef struct {
    State* table;
    bool* occupied; // Tracks if a slot is occupied
    size_t capacity;
} VisitedSet;

VisitedSet* create_visited_set(size_t capacity) {
    VisitedSet* vs = (VisitedSet*)malloc(sizeof(VisitedSet));
    vs->capacity = capacity;
    // Use calloc to initialize states to zero and occupied to false
    vs->table = (State*)calloc(capacity, sizeof(State));
    vs->occupied = (bool*)calloc(capacity, sizeof(bool));
     if (!vs->table || !vs->occupied) {
         perror("Failed to allocate visited set");
         exit(EXIT_FAILURE);
     }
    return vs;
}

// Simple hash function for State
size_t hash_state(const State* s, size_t capacity) {
    size_t hash = 5381;
    hash = ((hash << 5) + hash) + s->ore;
    hash = ((hash << 5) + hash) + s->clay;
    hash = ((hash << 5) + hash) + s->obsidian;
    hash = ((hash << 5) + hash) + s->geode;
    hash = ((hash << 5) + hash) + s->ore_robots;
    hash = ((hash << 5) + hash) + s->clay_robots;
    hash = ((hash << 5) + hash) + s->obsidian_robots;
    hash = ((hash << 5) + hash) + s->geode_robots;
    hash = ((hash << 5) + hash) + s->time_left;
    return hash % capacity;
}

// Returns true if added, false if already present
bool insert_visited(VisitedSet* vs, State s) {
    size_t index = hash_state(&s, vs->capacity);
    size_t start_index = index;

    while (vs->occupied[index]) {
        // Check if the state at this index is identical
        if (memcmp(&vs->table[index], &s, sizeof(State)) == 0) {
            return false; // Already visited
        }
        index = (index + 1) % vs->capacity;
        if (index == start_index) {
            // Should not happen with a large enough table, but indicates table is full
            fprintf(stderr, "Hash table full!\n");
            exit(EXIT_FAILURE);
        }
    }

    // Found an empty slot
    vs->table[index] = s;
    vs->occupied[index] = true;
    return true;
}


void destroy_visited_set(VisitedSet* vs) {
    free(vs->table);
    free(vs->occupied);
    free(vs);
}


int max(int a, int b) { return a > b ? a : b; }
int max4(int a, int b, int c, int d) { return max(max(a, b), max(c, d)); }


int max_geode(const Blueprint* b, State initial_state) {
    int max_geodes = 0;
    Queue* q = create_queue(INITIAL_QUEUE_CAPACITY);
    VisitedSet* visited = create_visited_set(HASH_TABLE_SIZE);

    enqueue(q, initial_state);

    while (q->size > 0) {
        State s = dequeue(q);

        max_geodes = max(max_geodes, s.geode);

        if (s.time_left == 0) {
            continue;
        }

        // --- Pruning ---
        State current_s = s; // Work on a copy for pruning/visited check

        if (current_s.ore_robots >= b->max_ore_needed) {
             current_s.ore_robots = b->max_ore_needed;
        }
        if (current_s.clay_robots >= b->obsidian_clay_cost) {
             current_s.clay_robots = b->obsidian_clay_cost;
        }
        if (current_s.obsidian_robots >= b->geode_obsidian_cost) {
            current_s.obsidian_robots = b->geode_obsidian_cost;
        }

        // Cap resources based on maximum possible future spending
        // Max useful ore = time * max_ore_cost - current_robots * (time - 1)
        int max_ore_useful = current_s.time_left * b->max_ore_needed - current_s.ore_robots * (current_s.time_left - 1);
         if (current_s.ore >= max_ore_useful) {
            current_s.ore = max_ore_useful;
        }
        int max_clay_useful = current_s.time_left * b->obsidian_clay_cost - current_s.clay_robots * (current_s.time_left - 1);
        if (current_s.clay >= max_clay_useful) {
            current_s.clay = max_clay_useful;
        }
        int max_obsidian_useful = current_s.time_left * b->geode_obsidian_cost - current_s.obsidian_robots * (current_s.time_left - 1);
        if (current_s.obsidian >= max_obsidian_useful) {
            current_s.obsidian = max_obsidian_useful;
        }
        // --- End Pruning ---


        if (!insert_visited(visited, current_s)) {
            continue; // Already visited this pruned state
        }


        // --- Generate next states ---
        // Option 1: Wait (always possible)
        State next_s = s;
        next_s.time_left--;
        next_s.ore += s.ore_robots;
        next_s.clay += s.clay_robots;
        next_s.obsidian += s.obsidian_robots;
        next_s.geode += s.geode_robots;
        enqueue(q, next_s);


        // Option 2: Build Ore Robot
        if (s.ore >= b->ore_cost) {
            State build_s = s;
            build_s.time_left--;
            build_s.ore -= b->ore_cost;
            build_s.ore += s.ore_robots;
            build_s.clay += s.clay_robots;
            build_s.obsidian += s.obsidian_robots;
            build_s.geode += s.geode_robots;
            build_s.ore_robots++;
            enqueue(q, build_s);
        }

        // Option 3: Build Clay Robot
        if (s.ore >= b->clay_ore_cost) {
             State build_s = s;
            build_s.time_left--;
            build_s.ore -= b->clay_ore_cost;
            build_s.ore += s.ore_robots;
            build_s.clay += s.clay_robots;
            build_s.obsidian += s.obsidian_robots;
            build_s.geode += s.geode_robots;
            build_s.clay_robots++;
            enqueue(q, build_s);
        }

        // Option 4: Build Obsidian Robot
        if (s.ore >= b->obsidian_ore_cost && s.clay >= b->obsidian_clay_cost) {
            State build_s = s;
            build_s.time_left--;
            build_s.ore -= b->obsidian_ore_cost;
            build_s.clay -= b->obsidian_clay_cost;
            build_s.ore += s.ore_robots;
            build_s.clay += s.clay_robots;
            build_s.obsidian += s.obsidian_robots;
            build_s.geode += s.geode_robots;
            build_s.obsidian_robots++;
            enqueue(q, build_s);
        }

         // Option 5: Build Geode Robot
        if (s.ore >= b->geode_ore_cost && s.obsidian >= b->geode_obsidian_cost) {
            State build_s = s;
            build_s.time_left--;
            build_s.ore -= b->geode_ore_cost;
            build_s.obsidian -= b->geode_obsidian_cost;
             build_s.ore += s.ore_robots;
            build_s.clay += s.clay_robots;
            build_s.obsidian += s.obsidian_robots;
            build_s.geode += s.geode_robots;
            build_s.geode_robots++;
            enqueue(q, build_s);
        }
    }

    destroy_queue(q);
    destroy_visited_set(visited);
    return max_geodes;
}


int main() {
    FILE *infile = fopen("input.txt", "r");
    if (!infile) {
        perror("Error opening input file");
        return 1;
    }

    Blueprint blueprints[MAX_BLUEPRINTS];
    int blueprint_count = 0;
    char line[256]; // Buffer to read lines

     while (fgets(line, sizeof(line), infile) != NULL && blueprint_count < MAX_BLUEPRINTS) {
        Blueprint* b = &blueprints[blueprint_count];
        int scan_result = sscanf(line,
               "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian.",
               &b->id, &b->ore_cost, &b->clay_ore_cost, &b->obsidian_ore_cost, &b->obsidian_clay_cost, &b->geode_ore_cost, &b->geode_obsidian_cost);

        if (scan_result == 7) {
             b->max_ore_needed = max4(b->ore_cost, b->clay_ore_cost, b->obsidian_ore_cost, b->geode_ore_cost);
            blueprint_count++;
        } else {
             fprintf(stderr, "Warning: Failed to parse line: %s", line);
        }
    }
    fclose(infile);


    if (blueprint_count == 0) {
        fprintf(stderr, "No blueprints found in input file.\n");
        return 1;
    }

    long long total_prod = 1;
    int blueprints_to_process = (blueprint_count < 3) ? blueprint_count : 3;

    for (int i = 0; i < blueprints_to_process; ++i) {
        State initial_state = {0, 0, 0, 1, 0, 0, 0, 32, 0}; // ore, clay, obsidian, ore_r, clay_r, obs_r, geo_r, time, geode
        int result = max_geode(&blueprints[i], initial_state);
        // printf("Blueprint %d: Max Geodes = %d\n", blueprints[i].id, result); // Optional: print individual results
         if (result > 0) { // Avoid multiplying by zero if a blueprint yields 0
             total_prod *= result;
         } else if (total_prod != 0) {
            // If any result is 0, the final product is 0 (unless product already 0)
            total_prod = 0;
         }
    }

    printf("%lld\n", total_prod);

    return 0;
}
