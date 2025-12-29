
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define ROWS 5
#define COLS 13
#define HEAP_CAP 1000000
#define HASH_SIZE 2000003

typedef struct { int r, c; } Coord;
typedef struct { char grid[ROWS][COLS]; int energy; } State;
typedef struct HashNode { State state; struct HashNode *next; } HashNode;
typedef struct { State **data; int size; } MinHeap;

HashNode *hash_table[HASH_SIZE];

void heapify_up(MinHeap *h, int i) {
    while (i > 0) {
        int p = (i - 1) / 2;
        if (h->data[i]->energy < h->data[p]->energy) {
            State *t = h->data[i]; h->data[i] = h->data[p]; h->data[p] = t;
            i = p;
        } else break;
    }
}

void heapify_down(MinHeap *h, int i) {
    int s = i, l = 2 * i + 1, r = 2 * i + 2;
    if (l < h->size && h->data[l]->energy < h->data[s]->energy) s = l;
    if (r < h->size && h->data[r]->energy < h->data[s]->energy) s = r;
    if (s != i) {
        State *t = h->data[i]; h->data[i] = h->data[s]; h->data[s] = t;
        heapify_down(h, s);
    }
}

void heap_push(MinHeap *h, State *s) {
    h->data[h->size] = s;
    heapify_up(h, h->size++);
}

State* heap_pop(MinHeap *h) {
    State *m = h->data[0];
    h->data[0] = h->data[--h->size];
    heapify_down(h, 0);
    return m;
}

unsigned long hash_state(const char g[ROWS][COLS]) {
    unsigned long h = 5381;
    for (int i = 0; i < ROWS * COLS; i++) h = ((h << 5) + h) + ((char*)g)[i];
    return h % HASH_SIZE;
}

bool seen_or_update(State *s) {
    unsigned long idx = hash_state(s->grid);
    for (HashNode *curr = hash_table[idx]; curr; curr = curr->next) {
        if (memcmp(curr->state.grid, s->grid, ROWS * COLS) == 0) {
            if (s->energy >= curr->state.energy) return true;
            curr->state.energy = s->energy; return false;
        }
    }
    HashNode *n = malloc(sizeof(HashNode));
    memcpy(&n->state, s, sizeof(State));
    n->next = hash_table[idx];
    hash_table[idx] = n;
    return false;
}

int get_cost(char type) {
    if (type == 'A') return 1;
    if (type == 'B') return 10;
    if (type == 'C') return 100;
    return 1000;
}

int get_target_col(char type) { return 3 + (type - 'A') * 2; }

bool is_hallway_stop(int c) { return c == 3 || c == 5 || c == 7 || c == 9; }

int move_logic(const State *s, Coord start, Coord *moves, int *costs) {
    int count = 0, dists[ROWS][COLS];
    memset(dists, -1, sizeof(dists));
    Coord q[ROWS * COLS];
    int head = 0, tail = 0;
    char type = s->grid[start.r][start.c];
    int multiplier = get_cost(type);
    
    q[tail++] = start; dists[start.r][start.c] = 0;
    int dr[] = {-1, 1, 0, 0}, dc[] = {0, 0, -1, 1};
    
    while (head < tail) {
        Coord curr = q[head++];
        if (dists[curr.r][curr.c] > 0) {
            if (start.r == 1) {
                if (curr.c == get_target_col(type) && curr.r > 1) {
                    bool ready = true;
                    for (int r = curr.r + 1; r < ROWS - 1; r++)
                        if (s->grid[r][curr.c] != type) { ready = false; break; }
                    for (int r = 2; r < curr.r; r++)
                        if (s->grid[r][curr.c] != '.') { ready = false; break; }
                    if (ready) {
                        bool deepest = true;
                        for (int r = curr.r + 1; r < ROWS - 1; r++)
                            if (s->grid[r][curr.c] == '.') { deepest = false; break; }
                        if (deepest) { moves[count] = curr; costs[count++] = dists[curr.r][curr.c] * multiplier; }
                    }
                }
            } else {
                if (curr.r == 1 && !is_hallway_stop(curr.c)) {
                    moves[count] = curr; costs[count++] = dists[curr.r][curr.c] * multiplier;
                }
            }
        }
        for (int i = 0; i < 4; i++) {
            int nr = curr.r + dr[i], nc = curr.c + dc[i];
            if (nr >= 0 && nr < ROWS && nc >= 0 && nc < COLS && s->grid[nr][nc] == '.' && dists[nr][nc] == -1) {
                dists[nr][nc] = dists[curr.r][curr.c] + 1;
                q[tail++] = (Coord){nr, nc};
            }
        }
    }
    return count;
}

bool is_done(const State *s) {
    for (char t = 'A'; t <= 'D'; t++) {
        int c = get_target_col(t);
        for (int r = 2; r < ROWS - 1; r++) if (s->grid[r][c] != t) return false;
    }
    return true;
}

int main() {
    @autoreleasepool {
        State start = {0};
        FILE *f = fopen("input.txt", "r");
        for (int r = 0; r < ROWS; r++) {
            fgets(start.grid[r], COLS + 2, f);
            size_t len = strlen(start.grid[r]);
            if (len > 0 && start.grid[r][len - 1] == '\n') start.grid[r][--len] = '\0';
            for (size_t c = len; c < COLS; c++) start.grid[r][c] = ' ';
        }
        fclose(f);

        MinHeap h = { malloc(sizeof(State*) * HEAP_CAP), 0 };
        State *init = malloc(sizeof(State)); memcpy(init, &start, sizeof(State));
        heap_push(&h, init);
        seen_or_update(init);

        int min_energy = -1;
        while (h.size > 0) {
            State *curr = heap_pop(&h);
            if (is_done(curr)) { min_energy = curr->energy; break; }
            for (int r = 0; r < ROWS; r++) {
                for (int c = 0; c < COLS; c++) {
                    char type = curr->grid[r][c];
                    if (type < 'A' || type > 'D') continue;
                    if (r > 1 && c == get_target_col(type)) {
                        bool ok = true;
                        for (int br = r; br < ROWS - 1; br++) if (curr->grid[br][c] != type) { ok = false; break; }
                        if (ok) continue;
                    }
                    Coord moves[ROWS * COLS]; int costs[ROWS * COLS];
                    int n = move_logic(curr, (Coord){r, c}, moves, costs);
                    for (int i = 0; i < n; i++) {
                        State *next = malloc(sizeof(State));
                        memcpy(next, curr, sizeof(State));
                        next->grid[moves[i].r][moves[i].c] = type;
                        next->grid[r][c] = '.';
                        next->energy += costs[i];
                        if (seen_or_update(next)) free(next); else heap_push(&h, next);
                    }
                }
            }
        }
        printf("%d\n", min_energy);
    }
    return 0;
}

