
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <limits.h>

#define MAX_LINE_LEN 256
#define MAX_RULES 50
#define MAX_FIELDS 50
#define MAX_TICKETS 300
#define MAX_NAME_LEN 64

typedef struct {
    char name[MAX_NAME_LEN];
    int low1, high1;
    int low2, high2;
} Rule;

typedef struct {
    int values[MAX_FIELDS];
    int count;
} Ticket;

// --- Globals ---
Rule rules[MAX_RULES];
int rule_count = 0;

Ticket my_ticket;
Ticket nearby_tickets[MAX_TICKETS];
int nearby_ticket_count = 0;
int num_fields = 0;

// --- Helper Functions ---

bool is_valid_for_rule(int value, const Rule* rule) {
    return (value >= rule->low1 && value <= rule->high1) ||
           (value >= rule->low2 && value <= rule->high2);
}

bool is_valid_for_any_rule(int value) {
    for (int i = 0; i < rule_count; ++i) {
        if (is_valid_for_rule(value, &rules[i])) {
            return true;
        }
    }
    return false;
}

bool parse_ticket_line(char *line, Ticket *ticket) {
    int count = 0;
    char *token = strtok(line, ",");
    while (token != NULL && count < MAX_FIELDS) {
        ticket->values[count++] = atoi(token);
        token = strtok(NULL, ",");
    }
    ticket->count = count;
    // If num_fields is not set, set it. Otherwise verify consistency.
    if (num_fields == 0) {
        num_fields = count;
    } else if (num_fields != count) {
        fprintf(stderr, "Error: Inconsistent number of fields in tickets (%d vs %d)\n", num_fields, count);
        return false; // Indicate error or inconsistent data
    }
    return true;
}

// --- Core Logic ---

void solve_field_positions(int final_positions[MAX_RULES]) {
    unsigned long long possible_mask[MAX_RULES];
    unsigned long long all_fields_mask = (1ULL << num_fields) - 1;

    // 1. Initialize: All positions are possible for all rules initially
    for (int i = 0; i < rule_count; ++i) {
        possible_mask[i] = all_fields_mask;
        final_positions[i] = -1; // Not yet determined
    }

    // 2. Eliminate possibilities based on valid nearby tickets
    for (int t = 0; t < nearby_ticket_count; ++t) {
        for (int j = 0; j < num_fields; ++j) {
            int value = nearby_tickets[t].values[j];
            for (int i = 0; i < rule_count; ++i) {
                if (!is_valid_for_rule(value, &rules[i])) {
                    possible_mask[i] &= ~(1ULL << j); // Remove field j as possibility for rule i
                }
            }
        }
    }

    // 3. Iteratively assign unique positions
    int assigned_count = 0;
    unsigned long long assigned_fields_mask = 0;

    while (assigned_count < rule_count) {
        int found_rule = -1;
        int found_pos = -1;

        for (int i = 0; i < rule_count; ++i) {
            // Check if rule 'i' is unassigned and has exactly one possible position left
            if (final_positions[i] == -1) {
                 unsigned long long current_possible = possible_mask[i] & (~assigned_fields_mask);
                 // Check if exactly one bit is set using GCC/Clang builtin or a helper
                 #ifdef __GNUC__
                 if (__builtin_popcountll(current_possible) == 1) {
                     found_pos = __builtin_ctzll(current_possible); // Count trailing zeros gives the bit index
                 #else
                 // Manual popcount and find bit (less efficient)
                 int set_bits = 0;
                 int last_set_pos = -1;
                 for(int k=0; k<num_fields; ++k) {
                     if((current_possible >> k) & 1) {
                         set_bits++;
                         last_set_pos = k;
                     }
                 }
                 if (set_bits == 1) {
                     found_pos = last_set_pos;
                 #endif
                    found_rule = i;
                    break; // Found one rule to assign in this iteration
                }
            }
        }

        if (found_rule != -1) {
            final_positions[found_rule] = found_pos;
            assigned_fields_mask |= (1ULL << found_pos); // Mark this field position as assigned
            assigned_count++;

            // // Optional Optimization (already implicitly handled by checking against assigned_fields_mask):
            // // Remove the assigned position from other rules' possibilities
            // for (int k = 0; k < rule_count; ++k) {
            //     if (k != found_rule) {
            //         possible_mask[k] &= ~(1ULL << found_pos);
            //     }
            // }
        } else {
             // Should not happen with valid input if logic is correct
             fprintf(stderr, "Error: Could not resolve all field positions.\n");
             exit(EXIT_FAILURE);
        }
    }
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening input.txt");
        return 1;
    }

    char line[MAX_LINE_LEN];
    int section = 0; // 0: rules, 1: my ticket, 2: nearby tickets

    while (fgets(line, sizeof(line), file)) {
        // Remove trailing newline
        line[strcspn(line, "\n")] = 0;

        if (strlen(line) == 0) {
            section++;
            continue;
        }

        if (section == 0) { // Rules
             if (rule_count < MAX_RULES) {
                Rule *r = &rules[rule_count];
                // Example format: "departure location: 31-538 or 546-960"
                char *colon = strchr(line, ':');
                if (colon) {
                    *colon = '\0'; // Null-terminate name
                    strncpy(r->name, line, MAX_NAME_LEN - 1);
                    r->name[MAX_NAME_LEN - 1] = '\0'; // Ensure null termination

                    if (sscanf(colon + 1, " %d-%d or %d-%d", &r->low1, &r->high1, &r->low2, &r->high2) == 4) {
                         rule_count++;
                    } else {
                        fprintf(stderr, "Error parsing rule line: %s\n", line);
                    }
                }
             } else {
                 fprintf(stderr, "Error: Exceeded MAX_RULES\n");
             }
        } else if (section == 1) { // My Ticket
            if (strstr(line, "your ticket:") == NULL) {
                 // Need a mutable copy for strtok
                 char line_copy[MAX_LINE_LEN];
                 strncpy(line_copy, line, MAX_LINE_LEN-1);
                 line_copy[MAX_LINE_LEN-1] = '\0';
                 if (!parse_ticket_line(line_copy, &my_ticket)) {
                     fclose(file);
                     return 1;
                 }
            }
        } else if (section == 2) { // Nearby Tickets
            if (strstr(line, "nearby tickets:") == NULL) {
                // Need a mutable copy for strtok
                 char line_copy[MAX_LINE_LEN];
                 strncpy(line_copy, line, MAX_LINE_LEN-1);
                 line_copy[MAX_LINE_LEN-1] = '\0';

                 Ticket current_ticket;
                 if (!parse_ticket_line(line_copy, &current_ticket)) {
                     fclose(file);
                     return 1;
                 }

                // Validate the entire ticket before adding
                bool ticket_is_valid = true;
                for (int i = 0; i < current_ticket.count; ++i) {
                    if (!is_valid_for_any_rule(current_ticket.values[i])) {
                        ticket_is_valid = false;
                        break;
                    }
                }

                if (ticket_is_valid) {
                     if (nearby_ticket_count < MAX_TICKETS) {
                         nearby_tickets[nearby_ticket_count++] = current_ticket; // Struct copy
                     } else {
                          fprintf(stderr, "Error: Exceeded MAX_TICKETS\n");
                     }
                }
            }
        }
    }
    fclose(file);

    // --- Solve and Calculate ---
    if (num_fields == 0 || rule_count == 0 || my_ticket.count == 0) {
        fprintf(stderr, "Error: Incomplete input data parsed.\n");
        return 1;
    }
     if (num_fields > 64) {
        fprintf(stderr, "Error: Number of fields (%d) exceeds limit for bitmask (64).\n", num_fields);
        return 1;
    }
     if (rule_count != num_fields) {
         fprintf(stderr, "Warning: Number of rules (%d) does not match number of fields (%d).\n", rule_count, num_fields);
         // Proceeding anyway, but the assignment logic assumes they match
     }


    int final_positions[MAX_RULES];
    solve_field_positions(final_positions);

    unsigned long long departure_product = 1;
    for (int i = 0; i < rule_count; ++i) {
        if (strncmp(rules[i].name, "departure", 9) == 0) {
            int pos = final_positions[i];
            if (pos >= 0 && pos < my_ticket.count) {
                 departure_product *= (unsigned long long)my_ticket.values[pos];
            } else {
                 fprintf(stderr, "Error: Invalid position %d resolved for rule '%s'\n", pos, rules[i].name);
                 return 1; // Indicate error
            }
        }
    }

    printf("%llu\n", departure_product);

    return 0;
}
