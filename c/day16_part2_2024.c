
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

// --- Configuration ---
// Adjust MAX_N and MAX_M based on expected grid size from "input.txt"
#define MAX_N 150
#define MAX_M 150
// Heap size needs to accommodate all possible states (N * M * 4 directions)
#define HEAP_MAX_SIZE (MAX_N * MAX_M * 4 + 1)

// --- Data Structures ---
// State for Dijkstra's priority queue
typedef struct {
    int cost;
    int x;
    int y;
    int d; // 0: up, 1: right, 2: down, 3: left
} State;

// Structure to hold coordinates and direction for backtracking
typedef struct {
    int x;
    int y;
    int d;
} PointDir;

// --- Global Variables ---
char grid[MAX_N][MAX_M];
int n = 0, m = 0;
int sx = -1, sy = -1, ex = -1, ey = -1; // Start and End coordinates
// dist[x][y][d]: min cost to reach (x, y) facing direction d
int dist[MAX_N][MAX_M][4];
// vis[x][y][d]: state visited during backtracking
bool vis[MAX_N][MAX_M][4];
// used[x][y]: grid cell is part of any shortest path
bool used[MAX_N][MAX_M];

// Min-Heap for Dijkstra's priority queue
State heap[HEAP_MAX_SIZE];
int heap_size = 0;

// Stack for backtracking (stores PointDir)
PointDir rev_stack[HEAP_MAX_SIZE];
int rev_top = 0;

// Direction vectors (Up, Right, Down, Left)
const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, 1, 0, -1};

// --- Min-Heap Implementation ---
// Swap two State elements
void swap(State *a, State *b) {
    State temp = *a;
    *a = *b;
    *b = temp;
}

// Move element up the heap to maintain heap property
void heapify_up(int index) {
    while (index > 0) {
        int parent = (index - 1) / 2;
        // If child is smaller than parent, swap and continue up
        if (heap[index].cost < heap[parent].cost) {
            swap(&heap[index], &heap[parent]);
            index = parent;
        } else {
            break; // Heap property satisfied
        }
    }
}

// Move element down the heap to maintain heap property
void heapify_down(int index) {
     int smallest = index;
     while (1) {
         int current_smallest = smallest;
         int left = 2 * smallest + 1;
         int right = 2 * smallest + 2;

         // Find the smallest among node, left child, and right child
         if (left < heap_size && heap[left].cost < heap[current_smallest].cost) {
             current_smallest = left;
         }
         if (right < heap_size && heap[right].cost < heap[current_smallest].cost) {
             current_smallest = right;
         }

         // If the smallest is not the current node, swap and continue down
         if (current_smallest != smallest) {
             swap(&heap[smallest], &heap[current_smallest]);
             smallest = current_smallest;
         } else {
             break; // Heap property satisfied
         }
     }
 }

// Push a State onto the heap
void heap_push(State s) {
    // Assume HEAP_MAX_SIZE is large enough, no overflow check for brevity
    heap[heap_size] = s;
    heapify_up(heap_size);
    heap_size++;
}

// Pop the State with the minimum cost from the heap
State heap_pop() {
    // Assume heap is not empty when called
    State top = heap[0];
    heap[0] = heap[heap_size - 1]; // Move last element to root
    heap_size--;
    if (heap_size > 0) {
        heapify_down(0); // Restore heap property from root
    }
    return top;
}

// --- Main Logic ---
int main() {
    FILE *f = fopen("input.txt", "r");
    if (f == NULL) {
        // Error opening file, print nothing and exit
        return 1;
    }

    // Read Grid from file
    char line[MAX_M + 2]; // Allow space for newline and null terminator
    while (n < MAX_N && fgets(line, sizeof(line), f)) {
        // Remove trailing newline/carriage return
        line[strcspn(line, "\r\n")] = 0;
        // Determine grid width from first line
        if (m == 0) {
            m = strlen(line);
            if (m > MAX_M) m = MAX_M; // Prevent overflow if line is too long
        }
        // Skip empty lines or lines not matching expected width
        if (strlen(line) == 0 || m <= 0) continue;

        // Copy line to grid, find S and E
        strncpy(grid[n], line, m);
        grid[n][m] = '\0'; // Ensure null termination

        for (int c = 0; c < m; ++c) {
            if (grid[n][c] == 'S') { sx = n; sy = c; }
            else if (grid[n][c] == 'E') { ex = n; ey = c; }
        }
        n++; // Increment row count
    }
    fclose(f);

    // Check if grid is valid and S/E were found
    if (n == 0 || m == 0 || sx == -1 || ex == -1) {
         printf("0\n"); // Print 0 if grid invalid or S/E missing
         return 0;
    }

    // Initialize distances to infinity and visited/used flags to false
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            used[i][j] = false;
            for (int k = 0; k < 4; ++k) {
                dist[i][j][k] = INT_MAX;
                vis[i][j][k] = false;
            }
        }
    }

    // --- Dijkstra's Algorithm ---
    // Start state: at S, facing right (direction 1), cost 0
    dist[sx][sy][1] = 0;
    heap_push((State){0, sx, sy, 1});

    while (heap_size > 0) {
        State current = heap_pop();
        int cost = current.cost; int x = current.x; int y = current.y; int d = current.d;

        // If we found a shorter path to this state already, ignore
        if (cost > dist[x][y][d]) continue;

        // Explore Turning (cost + 1000)
        for (int i = 0; i < 2; ++i) { // i=0: turn right (+1), i=1: turn left (+3)
            int ndir = (d + (i == 0 ? 1 : 3)) % 4;
            int nc = cost + 1000;
            // Check for potential overflow (unlikely here) and if path is shorter
            if (nc >= 0 && nc < dist[x][y][ndir]) {
                 dist[x][y][ndir] = nc;
                 heap_push((State){nc, x, y, ndir});
            }
        }

        // Explore Moving Forward (cost + 1)
        int nx = x + dx[d]; int ny = y + dy[d];
        // Check bounds and if the next cell is not a wall ('#')
        if (nx >= 0 && nx < n && ny >= 0 && ny < m && grid[nx][ny] != '#') {
            int nc = cost + 1;
            // Check if path is shorter
            if (nc >= 0 && nc < dist[nx][ny][d]) {
                 dist[nx][ny][d] = nc;
                 heap_push((State){nc, nx, ny, d});
            }
        }
    }

    // --- Find Minimum Cost to Reach E ---
    int best = INT_MAX;
    for (int d = 0; d < 4; ++d) {
        if (dist[ex][ey][d] < best) best = dist[ex][ey][d];
    }

    // If E is unreachable, print 0
    if (best == INT_MAX) {
        printf("0\n");
        return 0;
    }

    // --- Backtracking from E ---
    // Push all end states (at E with 'best' cost) onto the backtracking stack
    for (int d = 0; d < 4; ++d) {
        if (dist[ex][ey][d] == best && !vis[ex][ey][d]) {
             vis[ex][ey][d] = true; // Mark state as visited for backtracking
             rev_stack[rev_top++] = (PointDir){ex, ey, d}; // Push state
        }
    }

    // Process states from the backtracking stack (DFS-like exploration of shortest paths)
    while (rev_top > 0) {
        PointDir current = rev_stack[--rev_top]; // Pop state
        int x = current.x; int y = current.y; int d = current.d;

        // Mark the cell (x, y) as used (part of at least one shortest path)
        used[x][y] = true;
        int costU = dist[x][y][d]; // Cost to reach this state

        // Check Predecessor: Did we arrive via Turning at (x, y)?
        // Check both possible turns (left/right) that could lead to direction 'd'
        for (int i = 0; i < 2; ++i) {
             int pd = (d + (i == 0 ? 1 : 3)) % 4; // Potential previous direction
             // Check if cost difference matches turning cost (1000)
             // Use >= 1000 to avoid potential underflow issues if costU is small
             if (costU >= 1000 && dist[x][y][pd] == costU - 1000 && !vis[x][y][pd]) {
                 vis[x][y][pd] = true; // Mark predecessor state visited
                 rev_stack[rev_top++] = (PointDir){x, y, pd}; // Push predecessor state
             }
        }

        // Check Predecessor: Did we arrive via Moving from (px, py)?
        int px = x - dx[d]; // Potential previous x
        int py = y - dy[d]; // Potential previous y

        // Check if previous cell (px, py) is valid
        if (px >= 0 && px < n && py >= 0 && py < m && grid[px][py] != '#') {
             // Check if cost difference matches moving cost (1)
             // Use > 0 to avoid potential underflow issues if costU is 0
             if (costU > 0 && dist[px][py][d] == costU - 1 && !vis[px][py][d]) {
                 vis[px][py][d] = true; // Mark predecessor state visited
                 rev_stack[rev_top++] = (PointDir){px, py, d}; // Push predecessor state
             }
        }
    }

    // --- Count Used Cells ---
    int count = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (used[i][j]) count++;
        }
    }
    printf("%d\n", count); // Print the final count

    return 0; // Successful execution
}
