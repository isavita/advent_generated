
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

// --- Data Structures ---

typedef struct {
    int x, y, z;
} Point;

// Comparison function for qsort
int compare_points(const void *a, const void *b) {
    Point *p1 = (Point *)a;
    Point *p2 = (Point *)b;
    if (p1->x != p2->x) return p1->x - p2->x;
    if (p1->y != p2->y) return p1->y - p2->y;
    return p1->z - p2->z;
}

// Simple dynamic array for Points
typedef struct {
    Point *data;
    int size;
    int capacity;
} Vector;

void init_vector(Vector *v, int capacity) {
    v->data = (Point *)malloc(capacity * sizeof(Point));
    if (!v->data) {
        perror("Failed to allocate vector data");
        exit(EXIT_FAILURE);
    }
    v->size = 0;
    v->capacity = capacity;
}

void add_point(Vector *v, Point p) {
    if (v->size >= v->capacity) {
        v->capacity = (v->capacity == 0) ? 16 : v->capacity * 2;
        Point *new_data = (Point *)realloc(v->data, v->capacity * sizeof(Point));
        if (!new_data) {
            perror("Failed to reallocate vector data");
            free(v->data); // Free old data before exiting
            exit(EXIT_FAILURE);
        }
        v->data = new_data;
    }
    v->data[v->size++] = p;
}

void free_vector(Vector *v) {
    free(v->data);
    v->data = NULL;
    v->size = 0;
    v->capacity = 0;
}

typedef struct {
    Vector beacons;
} Scanner;

// --- Rotations (Precomputed) ---

// 24 Rotation matrices (derived from permutations/signs with det=1)
const int rotations[24][3][3] = {
    {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, {{1, 0, 0}, {0, 0, -1}, {0, 1, 0}},
    {{1, 0, 0}, {0, -1, 0}, {0, 0, -1}}, {{1, 0, 0}, {0, 0, 1}, {0, -1, 0}},
    {{0, -1, 0}, {1, 0, 0}, {0, 0, 1}}, {{0, 0, 1}, {1, 0, 0}, {0, 1, 0}},
    {{0, 1, 0}, {1, 0, 0}, {0, 0, -1}}, {{0, 0, -1}, {1, 0, 0}, {0, -1, 0}},
    {{-1, 0, 0}, {0, -1, 0}, {0, 0, 1}}, {{-1, 0, 0}, {0, 0, -1}, {0, -1, 0}},
    {{-1, 0, 0}, {0, 1, 0}, {0, 0, -1}}, {{-1, 0, 0}, {0, 0, 1}, {0, 1, 0}},
    {{0, 1, 0}, {-1, 0, 0}, {0, 0, 1}}, {{0, 0, 1}, {-1, 0, 0}, {0, -1, 0}},
    {{0, -1, 0}, {-1, 0, 0}, {0, 0, -1}}, {{0, 0, -1}, {-1, 0, 0}, {0, 1, 0}},
    {{0, 0, -1}, {0, 1, 0}, {1, 0, 0}}, {{0, 1, 0}, {0, 0, 1}, {1, 0, 0}},
    {{0, 0, 1}, {0, -1, 0}, {1, 0, 0}}, {{0, -1, 0}, {0, 0, -1}, {1, 0, 0}},
    {{0, 0, -1}, {0, -1, 0}, {-1, 0, 0}}, {{0, -1, 0}, {0, 0, 1}, {-1, 0, 0}},
    {{0, 0, 1}, {0, 1, 0}, {-1, 0, 0}}, {{0, 1, 0}, {0, 0, -1}, {-1, 0, 0}}
};

Point rotate(Point p, int rot_idx) {
    const int (*rot)[3] = rotations[rot_idx];
    Point rp;
    rp.x = p.x * rot[0][0] + p.y * rot[0][1] + p.z * rot[0][2];
    rp.y = p.x * rot[1][0] + p.y * rot[1][1] + p.z * rot[1][2];
    rp.z = p.x * rot[2][0] + p.y * rot[2][1] + p.z * rot[2][2];
    return rp;
}

Point add(Point p1, Point p2) {
    return (Point){p1.x + p2.x, p1.y + p2.y, p1.z + p2.z};
}

Point subtract(Point p1, Point p2) {
    return (Point){p1.x - p2.x, p1.y - p2.y, p1.z - p2.z};
}

int manhattan(Point p1, Point p2) {
    return abs(p1.x - p2.x) + abs(p1.y - p2.y) + abs(p1.z - p2.z);
}

// --- Input Reading ---

Scanner* read_input(const char *filename, int *num_scanners) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        return NULL;
    }

    Scanner *scanners = NULL;
    int scanner_capacity = 0;
    *num_scanners = 0;

    char line[100];
    Scanner current_scanner;
    bool reading_scanner = false;

    while (fgets(line, sizeof(line), f)) {
        if (strstr(line, "--- scanner")) {
            if (reading_scanner) { // Save previous scanner
                 if (*num_scanners >= scanner_capacity) {
                    scanner_capacity = (scanner_capacity == 0) ? 4 : scanner_capacity * 2;
                    Scanner *new_scanners = (Scanner *)realloc(scanners, scanner_capacity * sizeof(Scanner));
                    if (!new_scanners) {
                        perror("Failed to realloc scanners array");
                        fclose(f);
                        // Free partially allocated data if necessary
                        for(int i=0; i<*num_scanners; ++i) free_vector(&scanners[i].beacons);
                        free(scanners);
                        return NULL;
                    }
                    scanners = new_scanners;
                }
                scanners[*num_scanners] = current_scanner;
                (*num_scanners)++;
            }
            init_vector(&current_scanner.beacons, 16);
            reading_scanner = true;
        } else if (strlen(line) > 1 && line[0] != '\n') { // Check length > 1 for safety
            Point p;
            if (sscanf(line, "%d,%d,%d", &p.x, &p.y, &p.z) == 3) {
                if (reading_scanner) {
                   add_point(&current_scanner.beacons, p);
                }
            }
        }
    }

    // Save the last scanner
    if (reading_scanner) {
         if (*num_scanners >= scanner_capacity) {
            scanner_capacity = (scanner_capacity == 0) ? 4 : scanner_capacity * 2;
            Scanner *new_scanners = (Scanner *)realloc(scanners, scanner_capacity * sizeof(Scanner));
             if (!new_scanners) {
                perror("Failed to realloc scanners array (last)");
                fclose(f);
                free_vector(&current_scanner.beacons);
                for(int i=0; i<*num_scanners; ++i) free_vector(&scanners[i].beacons);
                free(scanners);
                return NULL;
            }
            scanners = new_scanners;
        }
        scanners[*num_scanners] = current_scanner;
        (*num_scanners)++;
    }

    fclose(f);
    return scanners;
}


// --- Solve ---

int solve(Scanner *scanners, int num_scanners) {
    if (num_scanners == 0) return 0;

    Vector global_beacons;
    init_vector(&global_beacons, scanners[0].beacons.size);
    for (int i = 0; i < scanners[0].beacons.size; ++i) {
        add_point(&global_beacons, scanners[0].beacons.data[i]);
    }
    qsort(global_beacons.data, global_beacons.size, sizeof(Point), compare_points);


    Vector scanner_positions;
    init_vector(&scanner_positions, num_scanners);
    add_point(&scanner_positions, (Point){0, 0, 0});

    bool *aligned_mask = (bool *)calloc(num_scanners, sizeof(bool));
     if (!aligned_mask) {
        perror("Failed to allocate aligned_mask");
        free_vector(&global_beacons);
        free_vector(&scanner_positions);
        exit(EXIT_FAILURE);
    }
    aligned_mask[0] = true;
    int aligned_count = 1;

    Vector deltas; // Reusable vector for deltas
    init_vector(&deltas, 1000); // Initial capacity, will grow

    Vector rotated_beacons; // Reusable vector for rotated points
    init_vector(&rotated_beacons, 50); // Max beacons per scanner approx

    while (aligned_count < num_scanners) {
        bool found_match = false;
        for (int i = 1; i < num_scanners; ++i) {
            if (aligned_mask[i]) continue;

            for (int rot = 0; rot < 24; ++rot) {
                rotated_beacons.size = 0; // Reset without reallocating
                for (int k = 0; k < scanners[i].beacons.size; ++k) {
                    add_point(&rotated_beacons, rotate(scanners[i].beacons.data[k], rot));
                }

                deltas.size = 0; // Reset delta count vector
                for (int j = 0; j < rotated_beacons.size; ++j) {
                    for (int k = 0; k < global_beacons.size; ++k) {
                        add_point(&deltas, subtract(global_beacons.data[k], rotated_beacons.data[j]));
                    }
                }

                if (deltas.size == 0) continue;

                qsort(deltas.data, deltas.size, sizeof(Point), compare_points);

                Point current_delta = deltas.data[0];
                int current_count = 1;
                int max_count = 1;
                Point best_delta = current_delta;

                for (int k = 1; k < deltas.size; ++k) {
                    if (compare_points(&deltas.data[k], &current_delta) == 0) {
                        current_count++;
                    } else {
                        if (current_count > max_count) {
                            max_count = current_count;
                            best_delta = current_delta;
                        }
                        current_delta = deltas.data[k];
                        current_count = 1;
                    }
                }
                 // Check last group
                if (current_count > max_count) {
                    max_count = current_count;
                    best_delta = current_delta;
                }


                if (max_count >= 12) {
                    // Found alignment
                    add_point(&scanner_positions, best_delta);
                    aligned_mask[i] = true;
                    aligned_count++;

                    // Add new beacons to global set, ensuring uniqueness
                    Vector new_beacons_to_add;
                    init_vector(&new_beacons_to_add, rotated_beacons.size);
                    for(int k=0; k < rotated_beacons.size; ++k) {
                         add_point(&new_beacons_to_add, add(rotated_beacons.data[k], best_delta));
                    }
                    qsort(new_beacons_to_add.data, new_beacons_to_add.size, sizeof(Point), compare_points);


                    // Merge sorted new_beacons into sorted global_beacons
                    Vector merged_beacons;
                    init_vector(&merged_beacons, global_beacons.size + new_beacons_to_add.size);
                    int g_idx = 0, n_idx = 0;
                    while(g_idx < global_beacons.size || n_idx < new_beacons_to_add.size) {
                        Point to_add;
                        int cmp_res = 0;

                        if(g_idx < global_beacons.size && n_idx < new_beacons_to_add.size) {
                             cmp_res = compare_points(&global_beacons.data[g_idx], &new_beacons_to_add.data[n_idx]);
                        } else if (g_idx < global_beacons.size) {
                            cmp_res = -1; // Only global left
                        } else {
                             cmp_res = 1; // Only new left
                        }

                        if (cmp_res < 0) {
                            to_add = global_beacons.data[g_idx++];
                        } else if (cmp_res > 0) {
                            to_add = new_beacons_to_add.data[n_idx++];
                        } else { // Equal
                            to_add = global_beacons.data[g_idx++];
                            n_idx++; // Skip duplicate from new
                        }

                        // Add if it's the first element or different from the last added
                         if(merged_beacons.size == 0 || compare_points(&to_add, &merged_beacons.data[merged_beacons.size-1]) != 0) {
                            add_point(&merged_beacons, to_add);
                         }
                    }

                    free_vector(&global_beacons);
                    global_beacons = merged_beacons; // Takes ownership of merged data

                    free_vector(&new_beacons_to_add);
                    found_match = true;
                    break; // Go to next scanner
                }
            } // End rotations
            if (found_match) break; // Found alignment for scanner i, move to outer loop
        } // End scanner loop

        if (!found_match && aligned_count < num_scanners) {
             fprintf(stderr, "Error: Could not align all scanners. Stuck at %d / %d\n", aligned_count, num_scanners);
             // Clean up allocated memory before exiting or returning error code
             free_vector(&deltas);
             free_vector(&rotated_beacons);
             free_vector(&global_beacons);
             free_vector(&scanner_positions);
             free(aligned_mask);
             // Returning -1 to indicate an error might be better than exiting here
             return -1; // Indicate error
        }
    }

    // Calculate max Manhattan distance between scanner positions
    int max_dist = 0;
    for (int i = 0; i < scanner_positions.size; ++i) {
        for (int j = i + 1; j < scanner_positions.size; ++j) {
            int dist = manhattan(scanner_positions.data[i], scanner_positions.data[j]);
            if (dist > max_dist) {
                max_dist = dist;
            }
        }
    }

    // --- Cleanup ---
    free_vector(&deltas);
    free_vector(&rotated_beacons);
    free_vector(&global_beacons);
    free_vector(&scanner_positions);
    free(aligned_mask);

    return max_dist;
}

// --- Main ---

int main() {
    int num_scanners = 0;
    Scanner *scanners = read_input("input.txt", &num_scanners);

    if (scanners == NULL) {
        return 1; // Error occurred during reading
    }
     if (num_scanners == 0) {
         printf("0\n"); // Or handle as error - no scanners found
         return 0;
    }

    int result = solve(scanners, num_scanners);

    if (result != -1) { // Check if solve encountered an error
        printf("%d\n", result);
    } else {
         fprintf(stderr, "Solver failed to align all scanners.\n");
    }


    // Final cleanup of scanners array itself
    for (int i = 0; i < num_scanners; ++i) {
        free_vector(&scanners[i].beacons);
    }
    free(scanners);

    return (result == -1); // Return 1 if solve failed, 0 otherwise
}
