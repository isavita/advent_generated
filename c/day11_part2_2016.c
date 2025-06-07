
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#define MAX_MATERIALS 10
#define NUM_FLOORS 4
#define QUEUE_SIZE 2000000
#define HASH_SIZE 4000037

typedef struct {
    uint32_t floors[NUM_FLOORS];
    uint8_t elevator;
    uint16_t steps;
} State;

typedef struct {
    uint8_t g;
    uint8_t c;
} Pair;

typedef struct Node {
    uint64_t key;
    struct Node* next;
} Node;

char materials[MAX_MATERIALS][20];
int num_materials = 0;

State queue[QUEUE_SIZE];
int q_front = 0, q_rear = 0;

Node* hash_table[HASH_SIZE];
Node node_pool[QUEUE_SIZE];
int node_idx = 0;

int get_mat_id(const char* name) {
    for (int i = 0; i < num_materials; i++) {
        if (strcmp(materials[i], name) == 0) return i;
    }
    strcpy(materials[num_materials], name);
    return num_materials++;
}

int pair_cmp(const void* a, const void* b) {
    Pair p1 = *(Pair*)a, p2 = *(Pair*)b;
    if (p1.g != p2.g) return p1.g - p2.g;
    return p1.c - p2.c;
}

uint64_t get_key(const State* s) {
    Pair pairs[MAX_MATERIALS];
    for (int i = 0; i < num_materials; i++) {
        for (int f = 0; f < NUM_FLOORS; f++) {
            if ((s->floors[f] >> i) & 1) pairs[i].g = f;
            if ((s->floors[f] >> (i + MAX_MATERIALS)) & 1) pairs[i].c = f;
        }
    }
    qsort(pairs, num_materials, sizeof(Pair), pair_cmp);
    uint64_t key = s->elevator;
    for (int i = 0; i < num_materials; i++) {
        key = (key << 4) | (pairs[i].g << 2) | pairs[i].c;
    }
    return key;
}

bool is_visited(uint64_t key) {
    uint32_t h = key % HASH_SIZE;
    for (Node* curr = hash_table[h]; curr; curr = curr->next) {
        if (curr->key == key) return true;
    }
    Node* new_node = &node_pool[node_idx++];
    new_node->key = key;
    new_node->next = hash_table[h];
    hash_table[h] = new_node;
    return false;
}

bool is_floor_valid(uint32_t floor) {
    uint32_t gens = floor & ((1 << MAX_MATERIALS) - 1);
    uint32_t chips = floor >> MAX_MATERIALS;
    return gens == 0 || (chips & ~gens) == 0;
}

bool is_state_valid(const State* s) {
    return is_floor_valid(s->floors[s->elevator]) && is_floor_valid(s->floors[s->elevator - (s->steps - queue[q_front].steps)]);
}

int solve() {
    State initial_state = {0};
    FILE* fp = fopen("input.txt", "r");
    char line[256], *words[32];
    for (int f = 0; fgets(line, sizeof(line), fp); f++) {
        int n_words = 0;
        for (char *c = line; *c; ++c) if (*c == ',' || *c == '.' || *c == '-') *c = ' ';
        for (char* token = strtok(line, " \n"); token; token = strtok(NULL, " \n")) {
            words[n_words++] = token;
        }
        for (int i = 0; i < n_words; i++) {
            if (strcmp(words[i], "generator") == 0) {
                initial_state.floors[f] |= 1 << get_mat_id(words[i - 1]);
            } else if (strcmp(words[i], "microchip") == 0) {
                initial_state.floors[f] |= 1 << (get_mat_id(words[i - 2]) + MAX_MATERIALS);
            }
        }
    }
    fclose(fp);
    
    initial_state.floors[0] |= 1 << get_mat_id("elerium");
    initial_state.floors[0] |= 1 << (get_mat_id("elerium") + MAX_MATERIALS);
    initial_state.floors[0] |= 1 << get_mat_id("dilithium");
    initial_state.floors[0] |= 1 << (get_mat_id("dilithium") + MAX_MATERIALS);

    queue[q_rear++] = initial_state;
    is_visited(get_key(&initial_state));
    uint32_t final_mask = 0;
    for(int i=0; i<num_materials; ++i) final_mask |= (1 << i) | (1 << (i + MAX_MATERIALS));

    while (q_front < q_rear) {
        State curr = queue[q_front++];
        if (curr.floors[NUM_FLOORS - 1] == final_mask) return curr.steps;

        uint8_t elev = curr.elevator;
        int items[2 * MAX_MATERIALS], n_items = 0;
        for (int i = 0; i < 2 * MAX_MATERIALS; i++) {
            if ((curr.floors[elev] >> i) & 1) items[n_items++] = i;
        }

        for (int d = -1; d <= 1; d += 2) {
            int next_elev = elev + d;
            if (next_elev < 0 || next_elev >= NUM_FLOORS) continue;

            for (int i = 0; i < n_items; i++) {
                uint32_t move1 = 1 << items[i];
                State next = curr;
                next.elevator = next_elev;
                next.steps++;
                next.floors[elev] &= ~move1;
                next.floors[next_elev] |= move1;
                if (is_floor_valid(next.floors[next_elev]) && is_floor_valid(next.floors[elev])) {
                    if (!is_visited(get_key(&next))) queue[q_rear++] = next;
                }
                
                for (int j = i + 1; j < n_items; j++) {
                    uint32_t move2 = move1 | (1 << items[j]);
                    State next2 = curr;
                    next2.elevator = next_elev;
                    next2.steps++;
                    next2.floors[elev] &= ~move2;
                    next2.floors[next_elev] |= move2;
                    if (is_floor_valid(next2.floors[next_elev]) && is_floor_valid(next2.floors[elev])) {
                       if (!is_visited(get_key(&next2))) queue[q_rear++] = next2;
                    }
                }
            }
        }
    }
    return -1;
}

int main() {
    printf("%d\n", solve());
    return 0;
}
