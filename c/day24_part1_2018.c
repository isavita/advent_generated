
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_GROUPS 50
#define MAX_MODIFIERS 10
#define BUFFER_SIZE 256
#define MAX_TYPE_LEN 20

typedef struct Group {
    int id; // Unique ID for debugging/tracking
    int army_id; // 1 for Immune System, 2 for Infection
    int units;
    int hit_points;
    int attack_damage;
    char attack_type[MAX_TYPE_LEN];
    int initiative;
    char immunities[MAX_MODIFIERS][MAX_TYPE_LEN];
    int num_immunities;
    char weaknesses[MAX_MODIFIERS][MAX_TYPE_LEN];
    int num_weaknesses;

    struct Group *target;
    struct Group *attacker;
    int target_priority; // Cache damage for target selection
} Group;

Group* all_groups[MAX_GROUPS];
int num_all_groups = 0;

int get_effective_power(const Group *g) {
    return g->units * g->attack_damage;
}

int has_modifier(const char list[][MAX_TYPE_LEN], int count, const char* type) {
    for (int i = 0; i < count; ++i) {
        if (strcmp(list[i], type) == 0) {
            return 1;
        }
    }
    return 0;
}

int calculate_damage(const Group *attacker, const Group *defender) {
    if (attacker->units <= 0 || defender->units <= 0) {
        return 0;
    }
    if (has_modifier(defender->immunities, defender->num_immunities, attacker->attack_type)) {
        return 0;
    }
    int power = get_effective_power(attacker);
    if (has_modifier(defender->weaknesses, defender->num_weaknesses, attacker->attack_type)) {
        return power * 2;
    }
    return power;
}

// Sort for target selection: effective power (desc), initiative (desc)
int compare_groups_target_selection(const void *a, const void *b) {
    const Group *group_a = *(const Group **)a;
    const Group *group_b = *(const Group **)b;

    int ep_a = get_effective_power(group_a);
    int ep_b = get_effective_power(group_b);

    if (ep_a != ep_b) {
        return ep_b - ep_a; // Higher effective power first
    }
    return group_b->initiative - group_a->initiative; // Higher initiative first
}

// Sort for attack order: initiative (desc)
int compare_groups_initiative(const void *a, const void *b) {
    const Group *group_a = *(const Group **)a;
    const Group *group_b = *(const Group **)b;
    return group_b->initiative - group_a->initiative; // Higher initiative first
}

void parse_modifiers(Group *g, const char *mod_str) {
    g->num_immunities = 0;
    g->num_weaknesses = 0;
    if (!mod_str) return;

    char buffer[BUFFER_SIZE];
    strncpy(buffer, mod_str, BUFFER_SIZE - 1);
    buffer[BUFFER_SIZE - 1] = '\0';

    char *current_section = buffer;
    while (current_section && *current_section) {
        char *next_semicolon = strchr(current_section, ';');
        if (next_semicolon) {
            *next_semicolon = '\0'; // Terminate current section
        }

        char *types_str = NULL;
        char (*target_list)[MAX_TYPE_LEN] = NULL;
        int *target_count = NULL;

        if (strncmp(current_section, "immune to ", 10) == 0) {
            types_str = current_section + 10;
            target_list = g->immunities;
            target_count = &g->num_immunities;
        } else if (strncmp(current_section, "weak to ", 8) == 0) {
             types_str = current_section + 8;
             target_list = g->weaknesses;
             target_count = &g->num_weaknesses;
        }

        if (types_str && target_list && target_count) {
            char *token = strtok(types_str, ", ");
            while (token && *target_count < MAX_MODIFIERS) {
                 // Trim leading/trailing spaces (though strtok usually handles this)
                while (isspace(*token)) token++;
                char* end = token + strlen(token) - 1;
                while(end > token && isspace(*end)) end--;
                *(end + 1) = '\0';

                if (strlen(token) > 0 && strlen(token) < MAX_TYPE_LEN) {
                   strncpy(target_list[*target_count], token, MAX_TYPE_LEN -1);
                   target_list[*target_count][MAX_TYPE_LEN-1] = '\0';
                   (*target_count)++;
                }
                token = strtok(NULL, ", ");
            }
        }

        current_section = next_semicolon ? next_semicolon + 1 : NULL;
         // Skip leading spaces for the next section if necessary
        while (current_section && isspace(*current_section)) {
            current_section++;
        }
    }
}


void load_input(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Error opening input file");
        exit(EXIT_FAILURE);
    }

    char line[BUFFER_SIZE];
    int current_army_id = 0;
    int group_counter = 0;

    while (fgets(line, sizeof(line), file)) {
        // Remove newline character
        line[strcspn(line, "\n")] = 0;

        if (strcmp(line, "Immune System:") == 0) {
            current_army_id = 1;
            continue;
        }
        if (strcmp(line, "Infection:") == 0) {
            current_army_id = 2;
            continue;
        }
        if (strlen(line) == 0 || current_army_id == 0) {
            continue;
        }

        Group *g = (Group *)malloc(sizeof(Group));
        if (!g) {
             perror("Failed to allocate memory for group");
             exit(EXIT_FAILURE);
        }
        memset(g, 0, sizeof(Group)); // Initialize struct fields

        g->id = ++group_counter;
        g->army_id = current_army_id;
        g->target = NULL;
        g->attacker = NULL;


        char *paren_open = strchr(line, '(');
        char *paren_close = strchr(line, ')');
        char modifiers_str[BUFFER_SIZE] = {0};

        if (paren_open && paren_close && paren_open < paren_close) {
            // Extract modifiers and modify the original line for sscanf
            size_t mod_len = paren_close - paren_open - 1;
            if (mod_len < sizeof(modifiers_str) -1 ) {
                strncpy(modifiers_str, paren_open + 1, mod_len);
                modifiers_str[mod_len] = '\0';
            }
             // Remove the parenthesis part from the main line by shifting
            memmove(paren_open -1, paren_close + 1, strlen(paren_close + 1) + 1); // Include null terminator
        }

        int n_scanned = sscanf(line, "%d units each with %d hit points with an attack that does %d %s damage at initiative %d",
               &g->units, &g->hit_points, &g->attack_damage, g->attack_type, &g->initiative);


        if (n_scanned != 5) {
            fprintf(stderr, "Error parsing line: %s\n", line);
            free(g);
            continue; // Skip malformed lines
        }

        if (strlen(modifiers_str) > 0) {
           parse_modifiers(g, modifiers_str);
        }


        if (num_all_groups < MAX_GROUPS) {
            all_groups[num_all_groups++] = g;
        } else {
             fprintf(stderr, "Exceeded MAX_GROUPS limit.\n");
             free(g);
             // Handle error appropriately, maybe exit or increase MAX_GROUPS
        }
    }

    fclose(file);
}

int count_alive_units(int army_id) {
    int total = 0;
    for (int i = 0; i < num_all_groups; ++i) {
        if (all_groups[i]->army_id == army_id && all_groups[i]->units > 0) {
            total += all_groups[i]->units;
        }
    }
    return total;
}

int main() {
    load_input("input.txt");

    Group* active_groups[MAX_GROUPS];
    Group* initiative_order[MAX_GROUPS];

    while (1) {
        int immune_alive = count_alive_units(1);
        int infection_alive = count_alive_units(2);

        if (immune_alive == 0 || infection_alive == 0) {
            break; // Combat ends
        }

        // --- Target Selection Phase ---
        int num_active = 0;
        for (int i = 0; i < num_all_groups; ++i) {
            all_groups[i]->target = NULL;   // Reset previous targets
            all_groups[i]->attacker = NULL; // Reset previous attackers
            if (all_groups[i]->units > 0) {
                 active_groups[num_active++] = all_groups[i];
            }
        }

        qsort(active_groups, num_active, sizeof(Group*), compare_groups_target_selection);

        for (int i = 0; i < num_active; ++i) {
            Group *attacker = active_groups[i];
            Group *best_target = NULL;
            int max_damage = 0;

            for (int j = 0; j < num_active; ++j) {
                Group *potential_target = active_groups[j];

                // Must be enemy, alive, and not already targeted
                if (potential_target->army_id == attacker->army_id || potential_target->attacker != NULL) {
                    continue;
                }

                int damage = calculate_damage(attacker, potential_target);
                if (damage == 0) {
                    continue;
                }

                if (damage > max_damage) {
                    max_damage = damage;
                    best_target = potential_target;
                } else if (damage == max_damage) {
                    if (!best_target || get_effective_power(potential_target) > get_effective_power(best_target)) {
                        best_target = potential_target;
                    } else if (get_effective_power(potential_target) == get_effective_power(best_target)) {
                        if (potential_target->initiative > best_target->initiative) {
                            best_target = potential_target;
                        }
                    }
                }
            }

            if (best_target) {
                attacker->target = best_target;
                best_target->attacker = attacker;
            }
        }

        // --- Attack Phase ---
        int num_initiative = 0;
         for (int i = 0; i < num_all_groups; ++i) {
             if (all_groups[i]->units > 0) {
                 initiative_order[num_initiative++] = all_groups[i];
             }
         }

        qsort(initiative_order, num_initiative, sizeof(Group*), compare_groups_initiative);

        int total_kills_this_round = 0;
        for (int i = 0; i < num_initiative; ++i) {
            Group *attacker = initiative_order[i];

            // Check if attacker is still alive (could have been killed earlier in the round)
             if (attacker->units <= 0 || attacker->target == NULL) {
                 continue;
             }
             // Target might have died before attacker's turn
             if (attacker->target->units <= 0) {
                 continue;
             }

            int damage = calculate_damage(attacker, attacker->target);
            int units_killed = damage / attacker->target->hit_points;

            if (units_killed > attacker->target->units) {
                units_killed = attacker->target->units; // Cannot kill more than available
            }

            attacker->target->units -= units_killed;
            total_kills_this_round += units_killed;
        }
         // Stalemate check (simple version: if no kills happened)
        if (total_kills_this_round == 0) {
             // If no kills happened and both sides still have units, it's a stalemate.
             // The original problem doesn't explicitly define stalemate outcome for Part 1,
             // but typically means neither side wins. Let's print 0 for stalemate.
             if (count_alive_units(1) > 0 && count_alive_units(2) > 0) {
                  printf("0\n"); // Indicate stalemate or infinite loop possibility
                  for (int i = 0; i < num_all_groups; ++i) {
                      free(all_groups[i]);
                  }
                 return 0;
             }
        }

        // Cleanup phase is implicit as we filter for units > 0 at the start of each loop.
    }

    // --- Calculate Result ---
    int final_units = count_alive_units(1) + count_alive_units(2);
    printf("%d\n", final_units);

    // --- Free Memory ---
    for (int i = 0; i < num_all_groups; ++i) {
        free(all_groups[i]);
    }

    return 0;
}
