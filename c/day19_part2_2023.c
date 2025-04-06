
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#define MAX_NAME_LEN 10
#define MAX_RULES 10
#define MAX_WORKFLOWS 600 // Adjusted based on typical input sizes
#define HASH_TABLE_SIZE (1 << 20) // Size for memoization table (needs to be power of 2)

typedef int64_t i64;

typedef struct {
    int min;
    int max;
} Range;

typedef struct {
    Range ranges[4]; // x, m, a, s
} Constraints;

typedef struct {
    char variable; // 'x', 'm', 'a', 's', or 0 for default rule
    char op;       // '<', '>', or 0
    int value;
    int dest_id;   // Workflow ID or -1 (A), -2 (R)
} Rule;

typedef struct {
    char name[MAX_NAME_LEN];
    Rule rules[MAX_RULES];
    int num_rules;
} Workflow;

typedef struct {
    int workflow_id;
    Constraints constraints;
} MemoKey;

typedef struct {
    MemoKey key;
    i64 value;
    bool occupied;
} MemoEntry;

Workflow workflows[MAX_WORKFLOWS];
int workflow_count = 0;
char workflow_name_map[MAX_WORKFLOWS][MAX_NAME_LEN];
int workflow_map_count = 0;

MemoEntry memo_table[HASH_TABLE_SIZE];

int get_workflow_id(const char* name) {
    if (strcmp(name, "A") == 0) return -1;
    if (strcmp(name, "R") == 0) return -2;
    for (int i = 0; i < workflow_map_count; ++i) {
        if (strcmp(workflow_name_map[i], name) == 0) {
            return i;
        }
    }
    if (workflow_map_count < MAX_WORKFLOWS) {
        strcpy(workflow_name_map[workflow_map_count], name);
        return workflow_map_count++;
    }
    fprintf(stderr, "Error: Too many workflows\n");
    exit(1);
}

int char_to_index(char v) {
    switch (v) {
        case 'x': return 0;
        case 'm': return 1;
        case 'a': return 2;
        case 's': return 3;
        default: return -1;
    }
}

// Simple hash function for constraints (FNV-1a inspired)
uint32_t hash_constraints(Constraints c) {
    uint32_t hash = 0x811c9dc5; // FNV offset basis
    for (int i = 0; i < 4; ++i) {
        hash ^= (uint32_t)c.ranges[i].min;
        hash *= 0x01000193; // FNV prime
        hash ^= (uint32_t)c.ranges[i].max;
        hash *= 0x01000193; // FNV prime
    }
    return hash;
}

uint32_t hash_key(MemoKey key) {
    uint32_t hash = hash_constraints(key.constraints);
    hash ^= (uint32_t)key.workflow_id;
    hash *= 0x01000193;
    return hash;
}

bool compare_keys(MemoKey k1, MemoKey k2) {
    if (k1.workflow_id != k2.workflow_id) return false;
    for (int i = 0; i < 4; ++i) {
        if (k1.constraints.ranges[i].min != k2.constraints.ranges[i].min ||
            k1.constraints.ranges[i].max != k2.constraints.ranges[i].max) {
            return false;
        }
    }
    return true;
}

bool memo_lookup(MemoKey key, i64* value) {
    uint32_t index = hash_key(key) & (HASH_TABLE_SIZE - 1);
    for (int i = 0; i < HASH_TABLE_SIZE; ++i) {
        uint32_t current_index = (index + i) & (HASH_TABLE_SIZE - 1);
        if (!memo_table[current_index].occupied) {
            return false; // Empty slot found
        }
        if (compare_keys(memo_table[current_index].key, key)) {
            *value = memo_table[current_index].value;
            return true; // Key found
        }
    }
    // Table is full and key not found (should not happen with good sizing)
    fprintf(stderr, "Warning: Hash table full or excessive collisions\n");
    return false;
}

void memo_insert(MemoKey key, i64 value) {
    uint32_t index = hash_key(key) & (HASH_TABLE_SIZE - 1);
    for (int i = 0; i < HASH_TABLE_SIZE; ++i) {
        uint32_t current_index = (index + i) & (HASH_TABLE_SIZE - 1);
        if (!memo_table[current_index].occupied) {
            memo_table[current_index].key = key;
            memo_table[current_index].value = value;
            memo_table[current_index].occupied = true;
            return;
        }
         // Optional: Handle overwrite if key already exists (should be caught by lookup)
        // if (compare_keys(memo_table[current_index].key, key)) {
        //     memo_table[current_index].value = value;
        //     return;
        // }
    }
    fprintf(stderr, "Error: Hash table full, cannot insert\n");
    // In a real scenario, might resize or handle error differently
}

i64 count_combinations(Constraints c) {
    i64 total = 1;
    for (int i = 0; i < 4; ++i) {
        i64 range_size = (i64)c.ranges[i].max - c.ranges[i].min + 1;
        if (range_size <= 0) return 0;
        total *= range_size;
    }
    return total;
}

i64 process(int workflow_id, Constraints constraints) {
    if (workflow_id == -2) return 0; // Rejected
    if (workflow_id == -1) return count_combinations(constraints); // Accepted

    for(int i=0; i<4; ++i) {
        if (constraints.ranges[i].min > constraints.ranges[i].max) return 0; // Invalid range
    }

    MemoKey key = {workflow_id, constraints};
    i64 cached_result;
    if (memo_lookup(key, &cached_result)) {
        return cached_result;
    }

    i64 result = 0;
    Workflow* wf = &workflows[workflow_id];
    Constraints current_constraints = constraints; // Work on a copy

    for (int i = 0; i < wf->num_rules; ++i) {
        Rule* rule = &wf->rules[i];

        if (rule->variable == 0) { // Default rule
            result += process(rule->dest_id, current_constraints);
            break; // Default rule is always last effective rule
        } else {
            int var_idx = char_to_index(rule->variable);
            Range var_range = current_constraints.ranges[var_idx];
            Range true_range = {0, -1}; // Initially invalid
            Range false_range = {0, -1}; // Initially invalid

            if (rule->op == '<') {
                true_range.min = var_range.min;
                true_range.max = rule->value - 1;
                false_range.min = rule->value;
                false_range.max = var_range.max;
            } else { // rule->op == '>'
                true_range.min = rule->value + 1;
                true_range.max = var_range.max;
                false_range.min = var_range.min;
                false_range.max = rule->value;
            }

            // Adjust ranges to be within the original var_range bounds
            true_range.min = (true_range.min > var_range.min) ? true_range.min : var_range.min;
            true_range.max = (true_range.max < var_range.max) ? true_range.max : var_range.max;
            false_range.min = (false_range.min > var_range.min) ? false_range.min : var_range.min;
            false_range.max = (false_range.max < var_range.max) ? false_range.max : var_range.max;

            // Process the true branch if the range is valid
            if (true_range.min <= true_range.max) {
                Constraints next_constraints = current_constraints;
                next_constraints.ranges[var_idx] = true_range;
                result += process(rule->dest_id, next_constraints);
            }

            // Continue with the false branch if the range is valid
            if (false_range.min <= false_range.max) {
                current_constraints.ranges[var_idx] = false_range;
                // Continue to the next rule with the updated constraints
            } else {
                // False branch is empty, no need to check further rules
                break;
            }
        }
    }

    memo_insert(key, result);
    return result;
}

int main() {
    FILE *f = fopen("input.txt", "r");
    if (!f) {
        perror("Error opening input.txt");
        return 1;
    }

    char line[1024];
    while (fgets(line, sizeof(line), f)) {
        if (line[0] == '\n' || line[0] == '\r') {
            break; // End of workflows section
        }
        line[strcspn(line, "\r\n")] = 0; // Remove newline

        char name[MAX_NAME_LEN];
        char* rules_str = strchr(line, '{');
        if (!rules_str) continue;

        *rules_str = '\0'; // Terminate name
        strcpy(name, line);
        rules_str++; // Move past '{'
        rules_str[strlen(rules_str) - 1] = '\0'; // Remove trailing '}'

        int current_wf_id = get_workflow_id(name);
        strcpy(workflows[current_wf_id].name, name);
        workflows[current_wf_id].num_rules = 0;

        char* rule_token = strtok(rules_str, ",");
        while (rule_token != NULL) {
            Rule* current_rule = &workflows[current_wf_id].rules[workflows[current_wf_id].num_rules++];
            char* colon = strchr(rule_token, ':');

            if (colon) {
                *colon = '\0'; // Split condition and destination
                char* dest_str = colon + 1;
                current_rule->dest_id = get_workflow_id(dest_str);

                current_rule->variable = rule_token[0];
                current_rule->op = rule_token[1];
                current_rule->value = atoi(rule_token + 2);
            } else {
                // Default rule
                current_rule->variable = 0;
                current_rule->op = 0;
                current_rule->value = 0;
                current_rule->dest_id = get_workflow_id(rule_token);
            }
            rule_token = strtok(NULL, ",");
        }
         workflow_count = workflow_map_count; // Keep track of actual workflows defined
    }

    fclose(f);

    // Initialize memoization table
    memset(memo_table, 0, sizeof(memo_table)); // Sets occupied to false

    Constraints initial_constraints;
    for (int i = 0; i < 4; ++i) {
        initial_constraints.ranges[i].min = 1;
        initial_constraints.ranges[i].max = 4000;
    }

    int start_workflow_id = get_workflow_id("in");
    if (start_workflow_id >= workflow_count) {
         fprintf(stderr, "Error: 'in' workflow not found\n");
         return 1;
    }

    i64 total_combinations = process(start_workflow_id, initial_constraints);

    printf("%lld\n", total_combinations);

    return 0;
}

