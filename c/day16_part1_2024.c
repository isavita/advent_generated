
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_ROWS 1000
#define MAX_COLS 1000
#define HEAP_MAX_SIZE (MAX_ROWS * MAX_COLS * 4) // Max possible states

typedef struct {
    int cost;
    int r;
    int c;
    int dir; // 0: E, 1: S, 2: W, 3: N
} State;

char grid[MAX_ROWS][MAX_COLS];
int rows = 0;
int cols = 0;
bool visited[MAX_ROWS][MAX_COLS][4]; // visited[r][c][dir]

State heap[HEAP_MAX_SIZE];
int heap_size = 0;

// Min-Heap functions
void swap(State *a, State *b) {
    State temp = *a;
    *a = *b;
    *b = temp;
}

void sift_up(int i) {
    while (i > 0) {
        int parent = (i - 1) / 2;
        if (heap[i].cost < heap[parent].cost) {
            swap(&heap[i], &heap[parent]);
            i = parent;
        } else {
            break;
        }
    }
}

void sift_down(int i) {
    int min_index = i;
    while (1) {
        int left = 2 * i + 1;
        int right = 2 * i + 2;

        if (left < heap_size && heap[left].cost < heap[min_index].cost) {
            min_index = left;
        }
        if (right < heap_size && heap[right].cost < heap[min_index].cost) {
            min_index = right;
        }

        if (min_index != i) {
            swap(&heap[i], &heap[min_index]);
            i = min_index;
        } else {
            break;
        }
    }
}

void push(State s) {
    if (heap_size >= HEAP_MAX_SIZE) {
        // Should not happen with defined limits
        fprintf(stderr, "Heap overflow\n");
        exit(1);
    }
    heap[heap_size] = s;
    heap_size++;
    sift_up(heap_size - 1);
}

State pop() {
    if (heap_size <= 0) {
        // Should not happen if path exists
        fprintf(stderr, "Heap underflow\n");
        exit(1);
    }
    State top = heap[0];
    heap[0] = heap[heap_size - 1];
    heap_size--;
    sift_down(0);
    return top;
}


int main() {
    FILE *f = fopen("input.txt", "r");
    if (f == NULL) {
        perror("Error opening input.txt");
        return 1;
    }

    int start_row = -1, start_col = -1;
    int end_row = -1, end_col = -1;

    char line[MAX_COLS + 2]; // +2 for potential \n and \0
    while (fgets(line, sizeof(line), f) != NULL) {
        // Remove trailing newline if present
        line[strcspn(line, "\n")] = 0;
        if (cols == 0) {
            cols = strlen(line);
        }
        if (rows < MAX_ROWS) {
            strcpy(grid[rows], line);
             for (int c = 0; c < cols; ++c) {
                if (grid[rows][c] == 'S') {
                    start_row = rows;
                    start_col = c;
                } else if (grid[rows][c] == 'E') {
                    end_row = rows;
                    end_col = c;
                }
            }
            rows++;
        } else {
            fprintf(stderr, "Warning: Exceeded MAX_ROWS\n");
            break; // Stop reading if grid is too large
        }
    }
    fclose(f);

    if (start_row == -1 || end_row == -1) {
        fprintf(stderr, "Start or End not found\n");
        return 1;
    }

    // Directions: E, S, W, N
    int dr[] = {0, 1, 0, -1};
    int dc[] = {1, 0, -1, 0};

    // Initial state: start position, cost 0, facing East (arbitrary, matches Python's initial push)
    push((State){0, start_row, start_col, 0});

    while (heap_size > 0) {
        State current = pop();
        int cost = current.cost;
        int r = current.r;
        int c = current.c;
        int dir = current.dir;

        if (visited[r][c][dir]) {
            continue;
        }
        visited[r][c][dir] = true;

        if (r == end_row && c == end_col) {
            printf("%d\n", cost);
            return 0;
        }

        // Try moving forward
        int nr = r + dr[dir];
        int nc = c + dc[dir];
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] != '#') {
             if (!visited[nr][nc][dir]) {
                 push((State){cost + 1, nr, nc, dir});
             }
        }

        // Try rotating clockwise
        int next_dir_cw = (dir + 1) % 4;
         if (!visited[r][c][next_dir_cw]) {
            push((State){cost + 1000, r, c, next_dir_cw});
         }


        // Try rotating counter-clockwise
        int next_dir_ccw = (dir - 1 + 4) % 4;
         if (!visited[r][c][next_dir_ccw]) {
            push((State){cost + 1000, r, c, next_dir_ccw});
         }
    }

    fprintf(stderr, "End not reachable\n");
    return 1; // Should not be reached if a path exists
}
