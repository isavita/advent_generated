
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

// --- Hash Set Implementation for Coordinates ---

typedef struct {
    int x;
    int y;
} Coord;

// Simple hash function for Coord
size_t hash_coord(Coord c, size_t capacity) {
    // Combine x and y using a simple method, ensure positive result before modulo
    long long hash_val = (long long)c.x * 31 + c.y;
    // Ensure positive value before modulo
    return (size_t)((hash_val % (long long)capacity + (long long)capacity) % (long long)capacity);
}

typedef struct HashNode {
    Coord key;
    struct HashNode* next;
} HashNode;

typedef struct {
    HashNode** buckets;
    size_t size;
    size_t capacity;
} HashSet;

// Create a new hash set
HashSet* HashSet_Create(size_t initial_capacity) {
    HashSet* set = (HashSet*)malloc(sizeof(HashSet));
    if (!set) return NULL;
    set->capacity = initial_capacity > 0 ? initial_capacity : 16; // Default capacity
    set->size = 0;
    set->buckets = (HashNode**)calloc(set->capacity, sizeof(HashNode*));
    if (!set->buckets) {
        free(set);
        return NULL;
    }
    return set;
}

// Free the hash set
void HashSet_Destroy(HashSet* set) {
    if (!set) return;
    for (size_t i = 0; i < set->capacity; ++i) {
        HashNode* current = set->buckets[i];
        while (current) {
            HashNode* next = current->next;
            free(current);
            current = next;
        }
    }
    free(set->buckets);
    free(set);
}

// Check if a coordinate exists in the set
bool HashSet_Contains(HashSet* set, Coord key) {
    size_t index = hash_coord(key, set->capacity);
    HashNode* current = set->buckets[index];
    while (current) {
        if (current->key.x == key.x && current->key.y == key.y) {
            return true;
        }
        current = current->next;
    }
    return false;
}

// Insert a coordinate into the set (returns false if already exists)
// Note: Does not handle resizing for simplicity in this context.
// Assumes initial capacity is large enough or collisions are acceptable.
bool HashSet_Insert(HashSet* set, Coord key) {
    size_t index = hash_coord(key, set->capacity);
    HashNode* current = set->buckets[index];
    while (current) {
        if (current->key.x == key.x && current->key.y == key.y) {
            return false; // Already exists
        }
        current = current->next;
    }

    // Insert new node at the beginning of the list
    HashNode* newNode = (HashNode*)malloc(sizeof(HashNode));
    if (!newNode) return false; // Allocation failure
    newNode->key = key;
    newNode->next = set->buckets[index];
    set->buckets[index] = newNode;
    set->size++;
    return true;
}

// Remove a coordinate from the set (returns false if not found)
bool HashSet_Remove(HashSet* set, Coord key) {
    size_t index = hash_coord(key, set->capacity);
    HashNode* current = set->buckets[index];
    HashNode* prev = NULL;

    while (current) {
        if (current->key.x == key.x && current->key.y == key.y) {
            if (prev) {
                prev->next = current->next;
            } else {
                set->buckets[index] = current->next;
            }
            free(current);
            set->size--;
            return true;
        }
        prev = current;
        current = current->next;
    }
    return false; // Not found
}

// --- Proposal Map Implementation ---

typedef struct {
    int count;
    Coord first_proposer;
} ProposalInfo;

typedef struct ProposalNode {
    Coord dest; // Key
    ProposalInfo info; // Value
    struct ProposalNode* next;
} ProposalNode;

typedef struct {
    ProposalNode** buckets;
    size_t capacity;
    // No size tracking needed here, just iterate through buckets later
} ProposalMap;

// Create a new proposal map
ProposalMap* ProposalMap_Create(size_t capacity) {
    ProposalMap* map = (ProposalMap*)malloc(sizeof(ProposalMap));
     if (!map) return NULL;
    map->capacity = capacity > 0 ? capacity : 16;
    map->buckets = (ProposalNode**)calloc(map->capacity, sizeof(ProposalNode*));
     if (!map->buckets) {
        free(map);
        return NULL;
    }
    return map;
}

// Clear and destroy the proposal map
void ProposalMap_Destroy(ProposalMap* map) {
    if (!map) return;
    for (size_t i = 0; i < map->capacity; ++i) {
        ProposalNode* current = map->buckets[i];
        while (current) {
            ProposalNode* next = current->next;
            free(current);
            current = next;
        }
    }
    free(map->buckets);
    free(map);
}

// Add or update a proposal for a destination
void ProposalMap_Propose(ProposalMap* map, Coord dest, Coord proposer) {
     size_t index = hash_coord(dest, map->capacity);
     ProposalNode* current = map->buckets[index];
     while(current) {
         if (current->dest.x == dest.x && current->dest.y == dest.y) {
             current->info.count++; // Increment count if destination exists
             return;
         }
         current = current->next;
     }

     // Destination not found, add new proposal
     ProposalNode* newNode = (ProposalNode*)malloc(sizeof(ProposalNode));
     if (!newNode) return; // Allocation failed
     newNode->dest = dest;
     newNode->info.count = 1;
     newNode->info.first_proposer = proposer;
     newNode->next = map->buckets[index];
     map->buckets[index] = newNode;
}


// --- Simulation Logic ---

// Relative positions of 8 neighbors
const int adj_dx[] = {-1, 0, 1, -1, 1, -1, 0, 1};
const int adj_dy[] = {-1, -1, -1, 0, 0, 1, 1, 1};

// Directions: N, S, W, E
// Check positions relative to current elf for each direction
const Coord checks[4][3] = {
    {{-1, -1}, {0, -1}, {1, -1}}, // N
    {{-1,  1}, {0,  1}, {1,  1}}, // S
    {{-1, -1}, {-1, 0}, {-1, 1}}, // W
    {{ 1, -1}, { 1, 0}, { 1, 1}}  // E
};
// Move vector for each direction
const Coord moves[4] = {
    {0, -1}, // N
    {0,  1}, // S
    {-1, 0}, // W
    { 1, 0}  // E
};

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    HashSet* elves = HashSet_Create(8192); // Start with a decent capacity
    if (!elves) {
        fclose(file);
        return 1;
    }

    char line[256];
    int y = 0;
    while (fgets(line, sizeof(line), file)) {
        int len = strlen(line);
        // Trim newline if present
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }
         if (len > 0 && line[len - 1] == '\r') { // Handle windows \r\n
            line[len - 1] = '\0';
            len--;
        }

        for (int x = 0; line[x] != '\0'; ++x) {
            if (line[x] == '#') {
                HashSet_Insert(elves, (Coord){x, y});
            }
        }
        y++;
    }
    fclose(file);

    int dir_order[4] = {0, 1, 2, 3}; // N, S, W, E indices
    const int ROUNDS = 10;

    // Temporary storage for current elves to iterate over
    Coord* current_elves_arr = NULL;
    size_t current_elves_capacity = 0;

    for (int round = 0; round < ROUNDS; ++round) {
        // --- Proposal Phase ---
        ProposalMap* proposals = ProposalMap_Create(elves->size); // Capacity heuristic
        if (!proposals) { free(current_elves_arr); HashSet_Destroy(elves); return 1; }

        // Allocate/Reallocate temporary array for current elves
        if (current_elves_capacity < elves->size) {
            current_elves_capacity = elves->size + elves->size / 2; // Grow by 50%
            Coord* new_arr = (Coord*)realloc(current_elves_arr, current_elves_capacity * sizeof(Coord));
            if (!new_arr) { free(current_elves_arr); ProposalMap_Destroy(proposals); HashSet_Destroy(elves); return 1; }
            current_elves_arr = new_arr;
        }

        // Copy current elf positions to the array
        size_t elf_idx = 0;
        for (size_t i = 0; i < elves->capacity; ++i) {
            HashNode* node = elves->buckets[i];
            while (node) {
                if (elf_idx < current_elves_capacity) { // Should always be true if realloc worked
                   current_elves_arr[elf_idx++] = node->key;
                } else { /* Error */ } // Should not happen
                node = node->next;
            }
        }

        // Iterate through the copied array
        for (size_t i = 0; i < elves->size; ++i) {
            Coord elf = current_elves_arr[i];
            bool has_neighbor = false;
            for (int j = 0; j < 8; ++j) {
                if (HashSet_Contains(elves, (Coord){elf.x + adj_dx[j], elf.y + adj_dy[j]})) {
                    has_neighbor = true;
                    break;
                }
            }

            if (!has_neighbor) continue; // No move proposed

            // Check directions in current order
            for (int k = 0; k < 4; ++k) {
                int dir_idx = dir_order[k];
                bool can_move = true;
                for (int check_idx = 0; check_idx < 3; ++check_idx) {
                     Coord check_pos = {elf.x + checks[dir_idx][check_idx].x, elf.y + checks[dir_idx][check_idx].y};
                     if (HashSet_Contains(elves, check_pos)) {
                         can_move = false;
                         break;
                     }
                }

                if (can_move) {
                    Coord dest = {elf.x + moves[dir_idx].x, elf.y + moves[dir_idx].y};
                    ProposalMap_Propose(proposals, dest, elf);
                    break; // Stop after first valid proposal
                }
            }
        }

        // --- Execution Phase ---
        int moves_made = 0;
        for (size_t i = 0; i < proposals->capacity; ++i) {
            ProposalNode* node = proposals->buckets[i];
            while (node) {
                if (node->info.count == 1) {
                    // Check if proposer hasn't already moved (shouldn't happen with this logic, but safe)
                    if(HashSet_Remove(elves, node->info.first_proposer)) {
                       HashSet_Insert(elves, node->dest);
                       moves_made++;
                    }
                }
                node = node->next;
            }
        }

        ProposalMap_Destroy(proposals); // Free proposal map for this round

        // Rotate direction order
        int first_dir = dir_order[0];
        for (int i = 0; i < 3; ++i) {
            dir_order[i] = dir_order[i + 1];
        }
        dir_order[3] = first_dir;

        // Part 2 check (not needed for this problem, but good structure)
        // if (moves_made == 0) {
        //     printf("Part 2: Round %d\n", round + 1);
        //     break;
        // }
    }

    free(current_elves_arr); // Free temp array

    // --- Calculate Empty Ground ---
    if (elves->size == 0) {
        printf("0\n");
        HashSet_Destroy(elves);
        return 0;
    }

    int min_x = INT_MAX, max_x = INT_MIN, min_y = INT_MAX, max_y = INT_MIN;
    bool first = true;

     for (size_t i = 0; i < elves->capacity; ++i) {
        HashNode* node = elves->buckets[i];
        while (node) {
            Coord elf = node->key;
            if (first) {
                min_x = max_x = elf.x;
                min_y = max_y = elf.y;
                first = false;
            } else {
                if (elf.x < min_x) min_x = elf.x;
                if (elf.x > max_x) max_x = elf.x;
                if (elf.y < min_y) min_y = elf.y;
                if (elf.y > max_y) max_y = elf.y;
            }
             node = node->next;
        }
     }

    long long width = (long long)max_x - min_x + 1;
    long long height = (long long)max_y - min_y + 1;
    long long total_tiles = width * height;
    long long empty_tiles = total_tiles - elves->size;

    printf("%lld\n", empty_tiles);

    HashSet_Destroy(elves);
    return 0;
}
