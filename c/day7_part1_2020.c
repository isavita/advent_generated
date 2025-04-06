
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_RULES 600
#define MAX_LINE_LEN 256
#define MAX_COLOR_LEN 30
#define MAX_CONTAINS 10

typedef struct {
    char name[MAX_COLOR_LEN];
    int contains[MAX_CONTAINS];
    int contains_count;
    int id;
} BagRule;

BagRule rules[MAX_RULES];
int num_colors = 0;
int shiny_gold_id = -1;
int memo[MAX_RULES]; // -1: uncomputed, 0: false, 1: true

int get_color_id(const char* name) {
    for (int i = 0; i < num_colors; ++i) {
        if (strcmp(rules[i].name, name) == 0) {
            return i;
        }
    }
    if (num_colors >= MAX_RULES) {
        fprintf(stderr, "Error: Too many colors\n");
        exit(1);
    }
    strcpy(rules[num_colors].name, name);
    rules[num_colors].contains_count = 0;
    rules[num_colors].id = num_colors;
    if (strcmp(name, "shiny gold") == 0) {
        shiny_gold_id = num_colors;
    }
    return num_colors++;
}

bool can_contain_shiny_gold(int bag_id) {
    if (bag_id == shiny_gold_id) {
        return true;
    }
    if (memo[bag_id] != -1) {
        return memo[bag_id] == 1;
    }

    bool found = false;
    for (int i = 0; i < rules[bag_id].contains_count; ++i) {
        if (can_contain_shiny_gold(rules[bag_id].contains[i])) {
            found = true;
            break;
        }
    }

    memo[bag_id] = found ? 1 : 0;
    return found;
}

int main() {
    FILE *f = fopen("input.txt", "r");
    if (!f) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), f)) {
        line[strcspn(line, "\n")] = 0; // Remove trailing newline

        char *contain_sep = strstr(line, " bags contain ");
        if (!contain_sep) continue;

        *contain_sep = '\0'; // Split line into bag color and contents
        char *bag_color_str = line;
        char *contents_str = contain_sep + strlen(" bags contain ");

        int outer_bag_id = get_color_id(bag_color_str);

        if (strcmp(contents_str, "no other bags.") == 0) {
            continue;
        }

        char *token = strtok(contents_str, ", ");
        while (token) {
            // Expecting " N color1 color2 bag(s)."
            // Skip number N
            if (atoi(token) == 0) { // Handle case where color starts the token
                 token = strtok(NULL, " "); // Skip 'bags.' or 'bag.' part after color name
                 continue;
            }

            char color1[MAX_COLOR_LEN];
            char color2[MAX_COLOR_LEN];
            
            token = strtok(NULL, " "); // color1
            if (!token) break; 
            strcpy(color1, token);

            token = strtok(NULL, " "); // color2
             if (!token) break; 
            strcpy(color2, token);
            
            char inner_color_name[MAX_COLOR_LEN];
            snprintf(inner_color_name, sizeof(inner_color_name), "%s %s", color1, color2);

            int inner_bag_id = get_color_id(inner_color_name);

            if (rules[outer_bag_id].contains_count < MAX_CONTAINS) {
                 rules[outer_bag_id].contains[rules[outer_bag_id].contains_count++] = inner_bag_id;
            } else {
                 fprintf(stderr, "Warning: Exceeded MAX_CONTAINS for %s\n", rules[outer_bag_id].name);
            }

            token = strtok(NULL, ", "); // Skip 'bags.' or 'bag.' part, move to next number or null
             if (token && (strstr(token, "bag") != NULL)){ // Handle pluralization properly skip bag[s].
                 token = strtok(NULL, ", ");
             }
        }
    }
    fclose(f);

    if (shiny_gold_id == -1) {
         printf("0\n"); // Shiny gold bag rule not found
         return 0;
    }

    for (int i = 0; i < num_colors; ++i) {
        memo[i] = -1;
    }

    int count = 0;
    for (int i = 0; i < num_colors; ++i) {
        if (i == shiny_gold_id) continue; // Don't count the shiny gold bag itself
        
        // Re-initialize memo for each top-level check to ensure correctness if structure changed
        // Actually, global memoization is correct and faster here. Resetting memo is not needed.
        // If can_contain_shiny_gold requires path tracking for cycles, then reset would be needed per call.
        // But here, memo stores the final reachability result.
        
        if (can_contain_shiny_gold(i)) {
            count++;
        }
    }

    printf("%d\n", count);

    return 0;
}
