
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LEN 150
#define MAX_RULES 2048 // Adjust if needed based on input size
#define HASH_MAP_SIZE (MAX_RULES * 2) // Simple hash map size
#define MAX_GRID_DIM 2000 // Estimated max grid dimension after 18 iterations

// --- Hash Map Implementation (Simple) ---
typedef struct Node {
    char *key;
    char *value;
    struct Node *next;
} Node;

Node* hash_map[HASH_MAP_SIZE];
Node* memo_map[HASH_MAP_SIZE]; // For memoization

unsigned int hash(const char *str) {
    unsigned long hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    return hash % HASH_MAP_SIZE;
}

void insert(Node** map, const char *key, const char *value) {
    unsigned int index = hash(key);
    Node *new_node = (Node*)malloc(sizeof(Node));
    if (!new_node) {
        perror("Failed to allocate node");
        exit(EXIT_FAILURE);
    }
    new_node->key = strdup(key);
    new_node->value = strdup(value);
     if (!new_node->key || !new_node->value) {
        perror("Failed to duplicate string");
        free(new_node->key); // Might be NULL already, but free is safe
        free(new_node->value);
        free(new_node);
        exit(EXIT_FAILURE);
    }
    new_node->next = map[index];
    map[index] = new_node;
}

char* lookup(Node** map, const char *key) {
    unsigned int index = hash(key);
    Node *current = map[index];
    while (current != NULL) {
        if (strcmp(current->key, key) == 0) {
            return current->value;
        }
        current = current->next;
    }
    return NULL;
}

void free_map(Node** map) {
    for (int i = 0; i < HASH_MAP_SIZE; i++) {
        Node *current = map[i];
        while (current != NULL) {
            Node *temp = current;
            current = current->next;
            free(temp->key);
            free(temp->value);
            free(temp);
        }
        map[i] = NULL;
    }
}

// --- Grid Manipulation ---

// Rotates a pattern string like "ab/cd" -> "ca/db" or "abc/def/ghi" -> "gda/heb/ifc"
char* rotate_str(const char* input_str) {
    int size = 0;
    if (strlen(input_str) == 5) size = 2; // "ab/cd"
    else if (strlen(input_str) == 11) size = 3; // "abc/def/ghi"
    else return NULL; // Should not happen with valid input

    int new_len = strlen(input_str);
    char* output_str = (char*)malloc(new_len + 1);
     if (!output_str) {
        perror("Failed to allocate memory for rotate");
        exit(EXIT_FAILURE);
    }

    int out_idx = 0;
    for (int x = 0; x < size; ++x) {
        for (int y = size - 1; y >= 0; --y) {
            // Calculate index in original string: y * (size + 1) + x
            output_str[out_idx++] = input_str[y * (size + 1) + x];
        }
        if (x < size - 1) {
            output_str[out_idx++] = '/';
        }
    }
    output_str[out_idx] = '\0';
    return output_str;
}

// Flips a pattern string horizontally like "ab/cd" -> "ba/dc"
char* flip_str(const char* input_str) {
    int size = 0;
     if (strlen(input_str) == 5) size = 2;
    else if (strlen(input_str) == 11) size = 3;
    else return NULL;

    int new_len = strlen(input_str);
    char* output_str = (char*)malloc(new_len + 1);
     if (!output_str) {
        perror("Failed to allocate memory for flip");
        exit(EXIT_FAILURE);
    }
    strcpy(output_str, input_str); // Copy original first

    int row_start = 0;
    for (int y = 0; y < size; ++y) {
        // Reverse the row segment in place
        char* left = output_str + row_start;
        char* right = output_str + row_start + size - 1;
        while (left < right) {
            char temp = *left;
            *left = *right;
            *right = temp;
            left++;
            right--;
        }
        row_start += (size + 1); // Move to the start of the next row
    }
    output_str[new_len] = '\0';
    return output_str;
}

char* enhance(const char* input_str) {
    char* memo_result = lookup(memo_map, input_str);
    if (memo_result) {
        return strdup(memo_result); // Return a copy
    }

    char* current_str = strdup(input_str);
    char* temp_str = NULL;
    char* result = NULL;

    // Check original and rotations
    for (int i = 0; i < 4; ++i) {
        result = lookup(hash_map, current_str);
        if (result) goto found;
        temp_str = rotate_str(current_str);
        free(current_str);
        current_str = temp_str;
    }

    // Flip and check rotations
    temp_str = flip_str(input_str); // Flip the original
    free(current_str);
    current_str = temp_str;

    for (int i = 0; i < 4; ++i) {
        result = lookup(hash_map, current_str);
        if (result) goto found;
        temp_str = rotate_str(current_str);
        free(current_str);
        current_str = temp_str;
    }

found:
    free(current_str); // Free the last transformed string

    if (result) {
        char* final_result = strdup(result);
        insert(memo_map, input_str, final_result); // Add original to memo
        return final_result;
    }

    fprintf(stderr, "Error: No rule found for pattern %s\n", input_str);
    exit(EXIT_FAILURE); // Should find a rule
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    // Initialize hash maps
    for (int i = 0; i < HASH_MAP_SIZE; ++i) {
        hash_map[i] = NULL;
        memo_map[i] = NULL;
    }

    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), file)) {
        char *input_pattern = strtok(line, " => ");
        char *output_pattern = strtok(NULL, " => \n"); // Also remove newline
        if (input_pattern && output_pattern) {
            insert(hash_map, input_pattern, output_pattern);
        }
    }
    fclose(file);

    // Initial Grid
    int grid_size = 3;
    char** grid = (char**)malloc(grid_size * sizeof(char*));
     if (!grid) { perror("malloc grid"); return 1; }
    grid[0] = strdup(".#.");
    grid[1] = strdup("..#");
    grid[2] = strdup("###");
     if (!grid[0] || !grid[1] || !grid[2]) { perror("strdup grid"); return 1;}


    for (int iter = 0; iter < 18; ++iter) {
        int sub_size;
        int new_sub_size;
        if (grid_size % 2 == 0) {
            sub_size = 2;
            new_sub_size = 3;
        } else {
            sub_size = 3;
            new_sub_size = 4;
        }

        int new_grid_size = (grid_size / sub_size) * new_sub_size;
        char** new_grid = (char**)malloc(new_grid_size * sizeof(char*));
         if (!new_grid) { perror("malloc new_grid"); return 1; }

        for (int i = 0; i < new_grid_size; ++i) {
            new_grid[i] = (char*)calloc(new_grid_size + 1, sizeof(char)); // Use calloc for zero-init
             if (!new_grid[i]) { perror("calloc new_grid row"); return 1; }
        }

        char sub_square_str[20]; // Max size needed "11c/11c/11c/11c" + null = 19+1 = 20

        for (int y = 0; y < grid_size; y += sub_size) {
            for (int x = 0; x < grid_size; x += sub_size) {
                // Extract sub-square into string format
                sub_square_str[0] = '\0';
                for (int dy = 0; dy < sub_size; ++dy) {
                    strncat(sub_square_str, grid[y + dy] + x, sub_size);
                    if (dy < sub_size - 1) {
                        strcat(sub_square_str, "/");
                    }
                }

                // Enhance the square
                char* new_square_str = enhance(sub_square_str);

                // Place the enhanced square into the new grid
                int new_y_base = (y / sub_size) * new_sub_size;
                int new_x_base = (x / sub_size) * new_sub_size;
                char* token = strtok(new_square_str, "/");
                int dy = 0;
                while(token != NULL && dy < new_sub_size) {
                    // Copy the row segment from the token
                   strncpy(new_grid[new_y_base + dy] + new_x_base, token, new_sub_size);
                   // Ensure null termination if strncpy didn't copy it (not strictly needed due to calloc + fixed size copy)
                   // new_grid[new_y_base + dy][new_x_base + new_sub_size] = '\0'; // Implicit from calloc

                   token = strtok(NULL, "/");
                   dy++;
                }
                free(new_square_str); // Free the result from enhance
            }
        }

        // Free old grid
        for (int i = 0; i < grid_size; ++i) {
            free(grid[i]);
        }
        free(grid);

        // Update grid
        grid = new_grid;
        grid_size = new_grid_size;
    }

    // Count '#'
    long long count = 0;
    for (int y = 0; y < grid_size; ++y) {
        for (int x = 0; x < grid_size; ++x) {
            if (grid[y][x] == '#') {
                count++;
            }
        }
    }
    printf("%lld\n", count);

    // Final Cleanup
    for (int i = 0; i < grid_size; ++i) {
        free(grid[i]);
    }
    free(grid);
    free_map(hash_map);
    free_map(memo_map);

    return 0;
}
