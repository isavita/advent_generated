
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LENGTH 256
#define MAX_WORKFLOWS 600
#define MAX_RULES 10
#define MAX_PARTS 300
#define MAX_NAME_LEN 4 // Max workflow name length + null terminator

typedef struct {
    int x, m, a, s;
} Part;

typedef struct {
    char category; // 'x', 'm', 'a', 's', or 0 for default rule
    char op;       // '<', '>', or 0 for default rule
    int value;
    short dest_idx; // Index in workflows array, -1 for 'A', -2 for 'R'
} Rule;

typedef struct {
    char name[MAX_NAME_LEN];
    Rule rules[MAX_RULES];
    int num_rules;
} Workflow;

Workflow workflows[MAX_WORKFLOWS];
int num_workflows = 0;
Part parts[MAX_PARTS];
int num_parts = 0;

// Simple map workflow name to index, handling 'A' and 'R'
short get_workflow_index(const char* name) {
    if (name[0] == 'A' && name[1] == '\0') return -1;
    if (name[0] == 'R' && name[1] == '\0') return -2;

    for (int i = 0; i < num_workflows; ++i) {
        if (strcmp(workflows[i].name, name) == 0) {
            return i;
        }
    }
    // Add new workflow if not found
    if (num_workflows < MAX_WORKFLOWS) {
        strncpy(workflows[num_workflows].name, name, MAX_NAME_LEN - 1);
        workflows[num_workflows].name[MAX_NAME_LEN - 1] = '\0';
        workflows[num_workflows].num_rules = 0;
        return num_workflows++;
    }
    return -3; // Error case
}

void parse_rule(Workflow* wf, const char* rule_str) {
    Rule* rule = &wf->rules[wf->num_rules++];
    char dest_name[MAX_NAME_LEN];
    char* colon_ptr = strchr(rule_str, ':');

    if (colon_ptr) {
        // Conditional rule (e.g., "a<2006:qkq")
        rule->category = rule_str[0];
        rule->op = rule_str[1];
        rule->value = atoi(rule_str + 2);
        strncpy(dest_name, colon_ptr + 1, MAX_NAME_LEN - 1);
        dest_name[MAX_NAME_LEN - 1] = '\0';
    } else {
        // Default rule (e.g., "rfg")
        rule->category = 0; // Indicate default rule
        rule->op = 0;
        rule->value = 0;
        strncpy(dest_name, rule_str, MAX_NAME_LEN - 1);
        dest_name[MAX_NAME_LEN - 1] = '\0';
    }
    rule->dest_idx = get_workflow_index(dest_name);
}

void parse_input(const char* filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    char line[MAX_LINE_LENGTH];
    bool parsing_workflows = true;

    while (fgets(line, sizeof(line), file)) {
        // Remove trailing newline
        line[strcspn(line, "\n")] = 0;

        if (strlen(line) == 0) {
            parsing_workflows = false;
            continue;
        }

        if (parsing_workflows) {
            char name[MAX_NAME_LEN];
            char* rules_start = strchr(line, '{');
            if (!rules_start) continue;

            *rules_start = '\0'; // Null-terminate name
            strncpy(name, line, MAX_NAME_LEN -1);
            name[MAX_NAME_LEN -1] = '\0';

            short wf_idx = get_workflow_index(name);
            if (wf_idx < 0) continue; // Should not happen for new workflow names

            Workflow* wf = &workflows[wf_idx];
            wf->num_rules = 0; // Reset rule count in case it was pre-allocated

            char* rule_str = strtok(rules_start + 1, ",}");
            while (rule_str) {
                if (strlen(rule_str) > 0) {
                   parse_rule(wf, rule_str);
                }
                rule_str = strtok(NULL, ",}");
            }
        } else {
            // Parse parts {x=...,m=...,a=...,s=...}
            if (num_parts < MAX_PARTS) {
                sscanf(line, "{x=%d,m=%d,a=%d,s=%d}",
                       &parts[num_parts].x,
                       &parts[num_parts].m,
                       &parts[num_parts].a,
                       &parts[num_parts].s);
                num_parts++;
            }
        }
    }
    fclose(file);
}

bool evaluate_part(const Part* part, short start_workflow_idx) {
    short current_idx = start_workflow_idx;

    while (current_idx >= 0) {
        const Workflow* wf = &workflows[current_idx];
        bool rule_matched = false;
        for (int i = 0; i < wf->num_rules; ++i) {
            const Rule* rule = &wf->rules[i];
            bool condition_met = false;

            if (rule->category == 0) { // Default rule
                condition_met = true;
            } else {
                int part_value;
                switch (rule->category) {
                    case 'x': part_value = part->x; break;
                    case 'm': part_value = part->m; break;
                    case 'a': part_value = part->a; break;
                    case 's': part_value = part->s; break;
                    default: part_value = 0; // Should not happen
                }

                if (rule->op == '<') {
                    condition_met = part_value < rule->value;
                } else if (rule->op == '>') {
                    condition_met = part_value > rule->value;
                }
            }

            if (condition_met) {
                current_idx = rule->dest_idx;
                rule_matched = true;
                break; // Go to next workflow/decision
            }
        }
         if (!rule_matched) return false; // Should not happen with default rule
         if (current_idx == -1) return true; // Accepted
         if (current_idx == -2) return false; // Rejected
    }
     // Should only exit loop via return A or R
    return false;
}


int main() {
    parse_input("input.txt");

    long long total_sum = 0;
    short in_idx = get_workflow_index("in");

    if (in_idx < 0) {
         fprintf(stderr, "Error: 'in' workflow not found.\n");
         return 1;
    }


    for (int i = 0; i < num_parts; ++i) {
        if (evaluate_part(&parts[i], in_idx)) {
            total_sum += parts[i].x + parts[i].m + parts[i].a + parts[i].s;
        }
    }

    printf("%lld\n", total_sum);

    return 0;
}
