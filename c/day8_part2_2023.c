
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_NODES 1000
#define MAX_INSTR_LEN 1000
#define MAX_LINE_LEN 100
#define NODE_NAME_LEN 3
#define HASH_SIZE 17576 // 26 * 26 * 26 for AAA to ZZZ

typedef struct {
    char name[NODE_NAME_LEN + 1];
    char left[NODE_NAME_LEN + 1];
    char right[NODE_NAME_LEN + 1];
} Node;

Node nodes[MAX_NODES];
int node_map[HASH_SIZE]; // Map name hash to index in nodes array
int node_count = 0;
char instructions[MAX_INSTR_LEN];
size_t instructions_length = 0;

int name_to_int(const char* name) {
    if (!name || strlen(name) != NODE_NAME_LEN) return -1;
    // Simple hash: treat AAA as base 26 number 0, AAB as 1, etc.
    // Assumes uppercase letters only.
    return (name[0] - 'A') * 26 * 26 + (name[1] - 'A') * 26 + (name[2] - 'A');
}

long long gcd(long long a, long long b) {
    while (b != 0) {
        long long temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

long long lcm(long long a, long long b) {
    if (a == 0 || b == 0) return 0;
    return (a / gcd(a, b)) * b; // Avoid potential overflow (a*b)/gcd
}

long long lcm_array(long long arr[], int n) {
    if (n == 0) return 0;
    if (n == 1) return arr[0];

    long long res = arr[0];
    for (int i = 1; i < n; i++) {
        res = lcm(res, arr[i]);
         if (res == 0 && (arr[0] != 0 || arr[i] != 0)) { // Handle case where intermediate LCM becomes 0 due to gcd issue with large numbers, although unlikely with long long
             fprintf(stderr, "Error: LCM became zero unexpectedly.\n");
             exit(EXIT_FAILURE);
         }
    }
    return res;
}

void parse_input(const char* filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    // Initialize node_map
    for (int i = 0; i < HASH_SIZE; ++i) {
        node_map[i] = -1;
    }

    // Read instructions
    if (fgets(instructions, MAX_INSTR_LEN, file) == NULL) {
        fprintf(stderr, "Error reading instructions\n");
        fclose(file);
        exit(EXIT_FAILURE);
    }
    instructions_length = strlen(instructions);
    if (instructions_length > 0 && instructions[instructions_length - 1] == '\n') {
        instructions[instructions_length - 1] = '\0';
        instructions_length--;
    }

    // Skip empty line
    char line[MAX_LINE_LEN];
    if (fgets(line, MAX_LINE_LEN, file) == NULL) {
        // Handle case where file ends abruptly after instructions (no empty line, no nodes)
         // Or potentially just ignore if it's expected behaviour for certain inputs
    }; // Read and discard the empty line

    // Read nodes
    while (fgets(line, MAX_LINE_LEN, file) != NULL && node_count < MAX_NODES) {
        if (strlen(line) < 10) continue; // Skip potentially malformed lines

        int matched = sscanf(line, "%3s = (%3s, %3s)",
               nodes[node_count].name,
               nodes[node_count].left,
               nodes[node_count].right);

        if (matched == 3) {
            nodes[node_count].name[NODE_NAME_LEN] = '\0';
            nodes[node_count].left[NODE_NAME_LEN] = '\0';
            nodes[node_count].right[NODE_NAME_LEN] = '\0';

            int hash_val = name_to_int(nodes[node_count].name);
            if (hash_val >= 0 && hash_val < HASH_SIZE) {
                 node_map[hash_val] = node_count;
            } else {
                 fprintf(stderr, "Invalid node name format or hash: %s\n", nodes[node_count].name);
                 // Optionally handle error more gracefully
            }
            node_count++;
        }
    }

    fclose(file);
}


long long solve() {
    int start_node_indices[MAX_NODES];
    int num_starts = 0;

    for (int i = 0; i < node_count; ++i) {
        if (nodes[i].name[NODE_NAME_LEN - 1] == 'A') {
            start_node_indices[num_starts++] = i;
        }
    }

    if (num_starts == 0) {
        return 0;
    }

    long long steps[MAX_NODES]; // Use MAX_NODES as upper bound for num_starts

    for (int i = 0; i < num_starts; ++i) {
        steps[i] = 0;
        int current_node_index = start_node_indices[i];
        while (nodes[current_node_index].name[NODE_NAME_LEN - 1] != 'Z') {
            char instruction = instructions[steps[i] % instructions_length];
            const char* next_node_name;

            if (instruction == 'L') {
                next_node_name = nodes[current_node_index].left;
            } else { // instruction == 'R'
                next_node_name = nodes[current_node_index].right;
            }

            int next_hash = name_to_int(next_node_name);
            if(next_hash < 0 || next_hash >= HASH_SIZE || node_map[next_hash] == -1){
                 fprintf(stderr, "Error: Node '%s' referenced but not found.\n", next_node_name);
                 exit(EXIT_FAILURE);
            }
            current_node_index = node_map[next_hash];
            steps[i]++;
             if (steps[i] < 0) { // Check for overflow, extremely unlikely with long long but good practice
                 fprintf(stderr, "Error: Step count overflow for start node %d\n", start_node_indices[i]);
                 exit(EXIT_FAILURE);
             }
        }
    }

    return lcm_array(steps, num_starts);
}

int main() {
    parse_input("input.txt");
    long long result = solve();
    printf("%lld\n", result);
    return 0;
}

