
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_GRID_SIZE 150
#define HASH_TABLE_SIZE 4000037 // Prime number larger than expected queue size
#define QUEUE_CAPACITY 2000000 // Estimated max size for queue during initial steps

typedef struct {
    long long x;
    long long y;
} Point;

// Basic Point Hash (using large primes)
unsigned int hash_point(Point p) {
    unsigned long long h = (unsigned long long)p.x * 378551 + (unsigned long long)p.y * 63689;
    return h % HASH_TABLE_SIZE;
}

// --- Hash Set (Open Addressing with Linear Probing) ---
Point hash_table[HASH_TABLE_SIZE];
bool occupied[HASH_TABLE_SIZE]; // Separate array to track occupation

void hash_set_clear() {
    memset(occupied, false, sizeof(occupied));
}

// Returns true if inserted, false if already present
bool hash_set_insert(Point p) {
    unsigned int index = hash_point(p);
    unsigned int start_index = index;

    while (occupied[index]) {
        if (hash_table[index].x == p.x && hash_table[index].y == p.y) {
            return false; // Already exists
        }
        index = (index + 1) % HASH_TABLE_SIZE;
        if (index == start_index) {
             fprintf(stderr, "Error: Hash table full!\n");
             exit(EXIT_FAILURE);
        }
    }
    hash_table[index] = p;
    occupied[index] = true;
    return true;
}
// --- End Hash Set ---


// Grid and Start Info
char grid[MAX_GRID_SIZE][MAX_GRID_SIZE];
int max_size = 0;
Point start = {-1, -1};

// Queues for BFS (Ping-Pong Buffers)
Point queue1[QUEUE_CAPACITY];
Point queue2[QUEUE_CAPACITY];


// Modulo function respecting negative coordinates
Point point_mod(Point p, int mod) {
    long long res_x = p.x % mod;
    long long res_y = p.y % mod;
    if (res_x < 0) res_x += mod;
    if (res_y < 0) res_y += mod;
    return (Point){res_x, res_y};
}

long long quadratic_function(long long n, long long a, long long b, long long c) {
    long long term1 = a;
    long long term2 = n * (b - a);
    long long term3 = (n * (n - 1) / 2) * (c - 2 * b + a);
    return term1 + term2 + term3;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    // Read grid and find start
    char line[MAX_GRID_SIZE + 2]; // +1 for newline, +1 for null terminator
    while (fgets(line, sizeof(line), file)) {
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }
        if (len == 0) continue; // Skip empty lines

        if (max_size >= MAX_GRID_SIZE) {
             fprintf(stderr, "Error: Grid exceeds MAX_GRID_SIZE\n");
             fclose(file);
             return 1;
        }

        strncpy(grid[max_size], line, MAX_GRID_SIZE);
        grid[max_size][MAX_GRID_SIZE - 1] = '\0'; // Ensure null termination if line was too long

        for (int x = 0; x < len; ++x) {
            if (grid[max_size][x] == 'S') {
                start = (Point){(long long)x, (long long)max_size};
                grid[max_size][x] = '.'; // Treat start as walkable
            }
        }
        max_size++;
    }
    fclose(file);

    if (start.x == -1) {
        fprintf(stderr, "Error: Start 'S' not found in grid.\n");
        return 1;
    }

    // --- BFS Simulation ---
    Point* current_queue = queue1;
    Point* next_queue = queue2;
    size_t current_count = 0;
    size_t next_count = 0;

    long long done[3];
    int done_count = 0;

    // Initialize queue with start point
    hash_set_clear();
    hash_set_insert(start);
    current_queue[0] = start;
    current_count = 1;

    int dx[] = {0, 0, 1, -1};
    int dy[] = {1, -1, 0, 0};

    int target_rem = (max_size - 1) / 2;

    for (int i = 0; ; ++i) {

        if (done_count < 3 && (i % max_size) == target_rem) {
             done[done_count++] = current_count;
             if (done_count == 3) {
                  // Optimization: We have enough data points, no need to simulate further
                  break;
             }
        }
         // Safety break if pattern doesn't emerge within reasonable steps
         // (3*max_size should be enough to establish the quadratic)
         if (i > 3 * max_size && done_count < 3) {
              fprintf(stderr, "Error: Did not find 3 points for quadratic fit within %d steps.\n", 3 * max_size);
              return 1;
         }


        hash_set_clear(); // Clear hash set for the *next* step's unique points
        next_count = 0;

        for (size_t j = 0; j < current_count; ++j) {
            Point p = current_queue[j];

            for (int k = 0; k < 4; ++k) {
                Point next_p = {p.x + dx[k], p.y + dy[k]};
                Point mod_p = point_mod(next_p, max_size);

                // Check if the tile in the base grid is not a rock
                if (grid[mod_p.y][mod_p.x] != '#') {
                    // Check if we've already added this *exact* coordinate to the next queue in *this* step
                    if (hash_set_insert(next_p)) {
                        if (next_count >= QUEUE_CAPACITY) {
                            fprintf(stderr, "Error: Queue capacity exceeded!\n");
                            return 1;
                        }
                        next_queue[next_count++] = next_p;
                    }
                }
            }
        }

        // Swap queues
        Point* temp_q = current_queue;
        current_queue = next_queue;
        next_queue = temp_q;
        current_count = next_count;

        if (current_count == 0) break; // Should not happen in this problem
    }

    // --- Extrapolation ---
    long long total_steps = 26501365;
    long long n = total_steps / max_size;

    long long result = quadratic_function(n, done[0], done[1], done[2]);

    printf("%lld\n", result);

    return 0;
}
