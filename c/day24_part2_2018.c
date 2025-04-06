
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_GROUPS_PER_ARMY 30
#define MAX_ATTACK_TYPES 10
#define MAX_LINE_LEN 256
#define MAX_TYPE_NAME_LEN 20

typedef struct Group Group;

struct Group {
    int id;
    int army_id; // 1 for Immune System, 2 for Infection
    int units;
    int hit_points;
    int attack_damage;
    int attack_type; // Index into attack_type_names
    int initiative;
    unsigned int immunities; // Bitmask
    unsigned int weaknesses; // Bitmask
    int initial_units;
    int initial_attack_damage;

    // Battle state
    Group *target;
    int targeted_by_attacker; // Flag to prevent multiple attackers targeting same group
};

char attack_type_names[MAX_ATTACK_TYPES][MAX_TYPE_NAME_LEN];
int num_attack_types = 0;

Group initial_immune_groups[MAX_GROUPS_PER_ARMY];
int initial_immune_count = 0;
Group initial_infection_groups[MAX_GROUPS_PER_ARMY];
int initial_infection_count = 0;

int get_or_add_attack_type(const char *name) {
    for (int i = 0; i < num_attack_types; ++i) {
        if (strcmp(attack_type_names[i], name) == 0) {
            return i;
        }
    }
    if (num_attack_types < MAX_ATTACK_TYPES) {
        strncpy(attack_type_names[num_attack_types], name, MAX_TYPE_NAME_LEN - 1);
        attack_type_names[num_attack_types][MAX_TYPE_NAME_LEN - 1] = '\0';
        return num_attack_types++;
    }
    // Should not happen with reasonable limits
    fprintf(stderr, "Error: Too many attack types\n");
    exit(1);
}

unsigned int parse_affinities(char *start, const char *prefix) {
    unsigned int mask = 0;
    char *aff_list = strstr(start, prefix);
    if (aff_list) {
        aff_list += strlen(prefix);
        char *end = strpbrk(aff_list, ";)");
        if (end) *end = '\0'; // Temporarily terminate the list

        char *token = strtok(aff_list, ", ");
        while (token) {
            if (strlen(token) > 0) {
                int type_id = get_or_add_attack_type(token);
                mask |= (1U << type_id);
            }
            token = strtok(NULL, ", ");
        }
        if (end) *end = (*end == '\0') ? ')' : ';'; // Restore char if needed
    }
    return mask;
}

void parse_line(char *line, int current_army_id) {
    Group g = {0};
    char attack_type_str[MAX_TYPE_NAME_LEN];
    char *details_start;

    if (sscanf(line, "%d units each with %d hit points", &g.units, &g.hit_points) != 2) {
        return; // Not a group line
    }
    g.initial_units = g.units;
    g.army_id = current_army_id;

    details_start = strstr(line, "with an attack that does ");
    if (!details_start || sscanf(details_start, "with an attack that does %d %s damage at initiative %d",
                              &g.attack_damage, attack_type_str, &g.initiative) != 3) {
        return; // Parsing failed
    }
    g.initial_attack_damage = g.attack_damage;
    g.attack_type = get_or_add_attack_type(attack_type_str);

    char *paren = strchr(line, '(');
    if (paren) {
        char *paren_end = strchr(paren, ')');
        if(paren_end) {
            // Create a mutable copy for strtok
            char affinity_str[MAX_LINE_LEN];
            strncpy(affinity_str, paren + 1, paren_end - paren - 1);
            affinity_str[paren_end - paren - 1] = '\0';

            g.immunities = parse_affinities(affinity_str, "immune to ");
            // Reset affinity_str for the second parse if needed
            strncpy(affinity_str, paren + 1, paren_end - paren - 1);
            affinity_str[paren_end - paren - 1] = '\0';
            g.weaknesses = parse_affinities(affinity_str, "weak to ");
        }
    }


    if (current_army_id == 1) {
        if (initial_immune_count < MAX_GROUPS_PER_ARMY) {
            g.id = initial_immune_count;
            initial_immune_groups[initial_immune_count++] = g;
        }
    } else if (current_army_id == 2) {
        if (initial_infection_count < MAX_GROUPS_PER_ARMY) {
            g.id = initial_infection_count;
            initial_infection_groups[initial_infection_count++] = g;
        }
    }
}

void parse_input(FILE *fp) {
    char line[MAX_LINE_LEN];
    int current_army_id = 0; // 0 = none, 1 = Immune, 2 = Infection

    while (fgets(line, sizeof(line), fp)) {
        line[strcspn(line, "\n")] = 0; // Remove newline
        if (strlen(line) == 0) continue;

        if (strcmp(line, "Immune System:") == 0) {
            current_army_id = 1;
        } else if (strcmp(line, "Infection:") == 0) {
            current_army_id = 2;
        } else if (current_army_id != 0) {
            parse_line(line, current_army_id);
        }
    }
}

int effective_power(const Group *g) {
    return g->units * g->attack_damage;
}

int calculate_damage(const Group *attacker, const Group *defender) {
    if (defender->immunities & (1U << attacker->attack_type)) {
        return 0;
    }
    int ep = effective_power(attacker);
    if (defender->weaknesses & (1U << attacker->attack_type)) {
        return ep * 2;
    }
    return ep;
}

int compare_target_selection(const void *a, const void *b) {
    const Group *ga = *(const Group **)a;
    const Group *gb = *(const Group **)b;
    int ep_a = effective_power(ga);
    int ep_b = effective_power(gb);
    if (ep_a != ep_b) return ep_b - ep_a; // Descending EP
    return gb->initiative - ga->initiative; // Descending Initiative
}

int compare_attack_order(const void *a, const void *b) {
    const Group *ga = *(const Group **)a;
    const Group *gb = *(const Group **)b;
    return gb->initiative - ga->initiative; // Descending Initiative
}


// Returns: 1 if Immune wins, 2 if Infection wins, 0 if stalemate
int simulate_battle(int boost, int *remaining_units) {
    Group immune_groups[MAX_GROUPS_PER_ARMY];
    int immune_count = initial_immune_count;
    Group infection_groups[MAX_GROUPS_PER_ARMY];
    int infection_count = initial_infection_count;

    memcpy(immune_groups, initial_immune_groups, sizeof(initial_immune_groups));
    memcpy(infection_groups, initial_infection_groups, sizeof(initial_infection_groups));

    for (int i = 0; i < immune_count; ++i) {
        immune_groups[i].units = immune_groups[i].initial_units;
        immune_groups[i].attack_damage = immune_groups[i].initial_attack_damage + boost;
    }
     for (int i = 0; i < infection_count; ++i) {
        infection_groups[i].units = infection_groups[i].initial_units;
        infection_groups[i].attack_damage = infection_groups[i].initial_attack_damage;
    }

    Group *all_groups_ptrs[MAX_GROUPS_PER_ARMY * 2];
    Group *army1_ptrs[MAX_GROUPS_PER_ARMY];
    Group *army2_ptrs[MAX_GROUPS_PER_ARMY];


    while (immune_count > 0 && infection_count > 0) {
        int total_units_before = 0;
        for(int i=0; i<immune_count; ++i) total_units_before += immune_groups[i].units;
        for(int i=0; i<infection_count; ++i) total_units_before += infection_groups[i].units;

        int current_army1_count = 0;
        int current_army2_count = 0;

        // Reset targets and targeted flag
        for (int i = 0; i < immune_count; ++i) {
            immune_groups[i].target = NULL;
            immune_groups[i].targeted_by_attacker = 0;
             if (immune_groups[i].units > 0) army1_ptrs[current_army1_count++] = &immune_groups[i];
        }
        for (int i = 0; i < infection_count; ++i) {
            infection_groups[i].target = NULL;
            infection_groups[i].targeted_by_attacker = 0;
             if (infection_groups[i].units > 0) army2_ptrs[current_army2_count++] = &infection_groups[i];
        }

        // Target Selection Phase
        // Sort potential attackers
        qsort(army1_ptrs, current_army1_count, sizeof(Group*), compare_target_selection);
        qsort(army2_ptrs, current_army2_count, sizeof(Group*), compare_target_selection);

        // Army 1 (Immune) selects targets
       for(int i=0; i < current_army1_count; ++i) {
           Group* attacker = army1_ptrs[i];
           Group* best_target = NULL;
           int max_damage = 0;
           int best_target_ep = 0;
           int best_target_init = 0;

           for(int j=0; j < current_army2_count; ++j) {
               Group* defender = army2_ptrs[j];
               if (defender->targeted_by_attacker) continue;

               int damage = calculate_damage(attacker, defender);
               if (damage == 0) continue;

               int defender_ep = effective_power(defender);

               if (damage > max_damage ||
                   (damage == max_damage && defender_ep > best_target_ep) ||
                   (damage == max_damage && defender_ep == best_target_ep && defender->initiative > best_target_init))
               {
                   max_damage = damage;
                   best_target = defender;
                   best_target_ep = defender_ep;
                   best_target_init = defender->initiative;
               }
           }
            if (best_target) {
                attacker->target = best_target;
                best_target->targeted_by_attacker = 1;
            }
       }

        // Army 2 (Infection) selects targets
        for(int i=0; i < current_army2_count; ++i) {
           Group* attacker = army2_ptrs[i];
           Group* best_target = NULL;
           int max_damage = 0;
           int best_target_ep = 0;
           int best_target_init = 0;

            for(int j=0; j < current_army1_count; ++j) {
                 Group* defender = army1_ptrs[j];
               if (defender->targeted_by_attacker) continue;

               int damage = calculate_damage(attacker, defender);
               if (damage == 0) continue;

               int defender_ep = effective_power(defender);

               if (damage > max_damage ||
                   (damage == max_damage && defender_ep > best_target_ep) ||
                   (damage == max_damage && defender_ep == best_target_ep && defender->initiative > best_target_init))
               {
                   max_damage = damage;
                   best_target = defender;
                   best_target_ep = defender_ep;
                   best_target_init = defender->initiative;
               }
           }
            if (best_target) {
                attacker->target = best_target;
                best_target->targeted_by_attacker = 1;
            }
       }

        // Attack Phase
        int all_groups_count = 0;
        for (int i = 0; i < immune_count; ++i) if (immune_groups[i].units > 0) all_groups_ptrs[all_groups_count++] = &immune_groups[i];
        for (int i = 0; i < infection_count; ++i) if (infection_groups[i].units > 0) all_groups_ptrs[all_groups_count++] = &infection_groups[i];

        qsort(all_groups_ptrs, all_groups_count, sizeof(Group*), compare_attack_order);

        for (int i = 0; i < all_groups_count; ++i) {
            Group *attacker = all_groups_ptrs[i];
            if (attacker->units <= 0 || attacker->target == NULL || attacker->target->units <= 0) {
                continue;
            }

            Group *defender = attacker->target;
            int damage = calculate_damage(attacker, defender);
            int units_killed = damage / defender->hit_points;

            defender->units -= units_killed;
            if (defender->units < 0) {
                defender->units = 0;
            }
        }

        // Clean up phase (implicitly handled by checking units > 0 in next loop iteration)
        // Update counts for the next round
        int next_immune_count = 0;
        for(int i=0; i<immune_count; ++i) {
             if (immune_groups[i].units > 0) {
                 if(i != next_immune_count) immune_groups[next_immune_count] = immune_groups[i]; // Compact array if needed, though only count matters here
                 next_immune_count++;
             }
        }
        immune_count = next_immune_count;


        int next_infection_count = 0;
         for(int i=0; i<infection_count; ++i) {
             if (infection_groups[i].units > 0) {
                  if(i != next_infection_count) infection_groups[next_infection_count] = infection_groups[i];
                 next_infection_count++;
             }
        }
       infection_count = next_infection_count;


        // Stalemate check
        int total_units_after = 0;
        for(int i=0; i<immune_count; ++i) total_units_after += immune_groups[i].units;
        for(int i=0; i<infection_count; ++i) total_units_after += infection_groups[i].units;

        if (total_units_after == total_units_before) {
            return 0; // Stalemate
        }
    }

    *remaining_units = 0;
    if (immune_count > 0) {
        for (int i = 0; i < immune_count; ++i) {
             if(immune_groups[i].units > 0) *remaining_units += immune_groups[i].units;
        }
        return 1; // Immune System wins
    } else if (infection_count > 0) {
         for (int i = 0; i < infection_count; ++i) {
             if(infection_groups[i].units > 0) *remaining_units += infection_groups[i].units;
        }
        return 2; // Infection wins
    } else {
        return 0; // Should technically not happen if stalemate is caught
    }
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        return 1;
    }

    parse_input(fp);
    fclose(fp);

    int boost = 0;
    while (1) {
        int remaining_units = 0;
        int winner = simulate_battle(boost, &remaining_units);

        if (winner == 1) { // Immune System wins
            printf("%d\n", remaining_units);
            break;
        }
        if (winner == 0 && boost > 0) { // Check stalemate after trying at least one boost
             // If boost 0 resulted in stalemate, maybe boost 1 works.
             // If boost > 0 results in stalemate, increasing boost likely won't help or will cause another stalemate.
             // Assume infinite loop or no solution if high boost leads to stalemate.
        }

        boost++;
         if (boost > 10000) { // Safety break for potentially infinite loops
            fprintf(stderr, "Error: Boost exceeded limit, possible infinite loop or no solution.\n");
             return 1;
        }
    }

    return 0;
}
