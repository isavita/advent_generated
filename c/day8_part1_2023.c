
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NODES 17576 // 26 * 26 * 26
#define MAX_INSTRUCTIONS 512
#define MAX_LINE_LEN 256

typedef struct {
    int left;
    int right;
} Node;

// Maps "XYZ" to an integer index 0-17575
int name_to_index(const char *name) {
    return (name[0] - 'A') * 26 * 26 + (name[1] - 'A') * 26 + (name[2] - 'A');
}

int main(void) {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char instructions[MAX_INSTRUCTIONS];
    char line[MAX_LINE_LEN];
    Node map[MAX_NODES];

    // Initialize map to indicate non-existent nodes if needed, though not strictly
    // necessary for this problem's guaranteed path.
    // Using -1, although 0 could work if indices are always non-negative.
    for(int i = 0; i < MAX_NODES; ++i) {
        map[i].left = -1;
        map[i].right = -1;
    }


    // Read instructions
    if (fgets(instructions, MAX_INSTRUCTIONS, file) == NULL) {
        fprintf(stderr, "Error reading instructions\n");
        fclose(file);
        return 1;
    }
    instructions[strcspn(instructions, "\n")] = 0; // Remove trailing newline
    int instruction_len = strlen(instructions);
    if (instruction_len == 0) {
         fprintf(stderr, "Error: Instructions are empty\n");
         fclose(file);
         return 1;
    }


    // Skip the blank line
    if (fgets(line, MAX_LINE_LEN, file) == NULL) {
         // Allow file to end here if map is empty (edge case)
    };

    // Read map definitions
    while (fgets(line, MAX_LINE_LEN, file)) {
        if (strlen(line) < 10) continue; // Skip empty or malformed lines

        char node_name[4], left_name[4], right_name[4];
        // Use sscanf for robust parsing
        if (sscanf(line, "%3s = (%3s, %3s)", node_name, left_name, right_name) == 3) {
             int node_idx = name_to_index(node_name);
             map[node_idx].left = name_to_index(left_name);
             map[node_idx].right = name_to_index(right_name);
        } else {
             fprintf(stderr, "Warning: Malformed map line: %s", line);
             // Decide whether to continue or exit based on requirements
        }
    }
    fclose(file);

    int current_index = name_to_index("AAA");
    int target_index = name_to_index("ZZZ");
    unsigned long long steps = 0;

    if (map[current_index].left == -1 && map[current_index].right == -1) {
         fprintf(stderr, "Error: Starting node AAA not found in map\n");
         return 1;
    }


    while (current_index != target_index) {
        char direction = instructions[steps % instruction_len];
        if (direction == 'L') {
            current_index = map[current_index].left;
        } else { // Assume 'R'
            current_index = map[current_index].right;
        }
         if (current_index < 0 || current_index >= MAX_NODES || (map[current_index].left == -1 && map[current_index].right == -1 && current_index != target_index) ) {
             fprintf(stderr, "Error: Traversed to an undefined or invalid node at step %llu\n", steps + 1);
             return 1; // Path leads to undefined node before target
         }
        steps++;
         // Prevent infinite loops in case of bad input or cycles not reaching ZZZ
         if (steps > 1000000000ULL) { // Arbitrarily large number
              fprintf(stderr, "Error: Exceeded maximum steps, possible infinite loop\n");
              return 1;
         }

    }

    printf("%llu\n", steps);

    return 0;
}
