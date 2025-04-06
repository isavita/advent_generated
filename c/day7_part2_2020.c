
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_BAG_TYPES 600 // Adjusted based on typical input sizes
#define MAX_LINE_LEN 256
#define INITIAL_CONTAINS_CAPACITY 4

// Structure to store inner bag info (using IDs)
typedef struct {
    int id;     // ID of the inner bag color
    int count;
} ContainedBag;

// Structure to store rules for one bag type
typedef struct {
    char *color_name;       // Store the original color name
    ContainedBag *contains; // Dynamically allocated array of contained bags
    int contains_count;     // Number of elements in the 'contains' array
    int contains_capacity;  // Capacity of the 'contains' array
    // Memoization flags/results
    int can_contain_gold_memo; // -1: unknown, 0: false, 1: true
    long long bags_inside_memo;    // -1: unknown, >=0: calculated result
} BagRule;

// Global storage
BagRule rules[MAX_BAG_TYPES];
int bag_count = 0;
int shiny_gold_id = -1;

// Function to get or create an ID for a color name
int get_bag_id(const char *color) {
    for (int i = 0; i < bag_count; ++i) {
        if (strcmp(color, rules[i].color_name) == 0) {
            return i;
        }
    }

    if (bag_count >= MAX_BAG_TYPES) {
        fprintf(stderr, "Error: Exceeded MAX_BAG_TYPES\n");
        exit(1);
    }

    int new_id = bag_count++;
    rules[new_id].color_name = strdup(color);
    if (!rules[new_id].color_name) {
        perror("strdup failed");
        exit(1);
    }
    rules[new_id].contains = malloc(INITIAL_CONTAINS_CAPACITY * sizeof(ContainedBag));
     if (!rules[new_id].contains) {
        perror("malloc failed");
        exit(1);
    }
    rules[new_id].contains_count = 0;
    rules[new_id].contains_capacity = INITIAL_CONTAINS_CAPACITY;
    rules[new_id].can_contain_gold_memo = -1;
    rules[new_id].bags_inside_memo = -1;

    if (strcmp(color, "shiny gold") == 0) {
        shiny_gold_id = new_id;
    }

    return new_id;
}

void add_contained_bag(int outer_id, int inner_id, int count) {
    BagRule *rule = &rules[outer_id];
    if (rule->contains_count >= rule->contains_capacity) {
        rule->contains_capacity *= 2;
        ContainedBag *new_contains = realloc(rule->contains, rule->contains_capacity * sizeof(ContainedBag));
        if (!new_contains) {
            perror("realloc failed");
            exit(1);
        }
        rule->contains = new_contains;
    }
    rule->contains[rule->contains_count].id = inner_id;
    rule->contains[rule->contains_count].count = count;
    rule->contains_count++;
}

// Part 1: Recursive function with memoization
int can_contain_shiny_gold_recursive(int current_bag_id) {
    if (current_bag_id == shiny_gold_id) {
        return 1; // Base case: it is shiny gold (though called externally excluding shiny gold itself)
    }
    if (rules[current_bag_id].can_contain_gold_memo != -1) {
        return rules[current_bag_id].can_contain_gold_memo;
    }

    BagRule *rule = &rules[current_bag_id];
    for (int i = 0; i < rule->contains_count; ++i) {
        if (can_contain_shiny_gold_recursive(rule->contains[i].id)) {
            rules[current_bag_id].can_contain_gold_memo = 1;
            return 1;
        }
    }

    rules[current_bag_id].can_contain_gold_memo = 0;
    return 0;
}

// Part 2: Recursive function with memoization
long long count_individual_bags_recursive(int current_bag_id) {
    if (rules[current_bag_id].bags_inside_memo != -1) {
        return rules[current_bag_id].bags_inside_memo;
    }

    long long total_bags = 0;
    BagRule *rule = &rules[current_bag_id];

    for (int i = 0; i < rule->contains_count; ++i) {
        int num = rule->contains[i].count;
        long long inner_total = count_individual_bags_recursive(rule->contains[i].id);
        total_bags += (long long)num + (long long)num * inner_total;
    }

    rules[current_bag_id].bags_inside_memo = total_bags;
    return total_bags;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove trailing newline

        char *contain_sep = strstr(line, " bags contain ");
        if (!contain_sep) continue;

        *contain_sep = '\0'; // Null-terminate the outer bag color
        char *outer_color = line;
        int outer_id = get_bag_id(outer_color);

        char *inner_part = contain_sep + strlen(" bags contain ");

        if (strcmp(inner_part, "no other bags.") == 0) {
            continue;
        }

        char *token;
        char *rest = inner_part;

        while ((token = strtok_r(rest, ",", &rest))) {
            while (isspace(*token)) token++; // Trim leading space

            int count;
            char adj[32], color[32];
            // Expecting "N adjective color bag(s)"
            if (sscanf(token, "%d %31s %31s", &count, adj, color) == 3) {
                 char inner_color_full[64];
                 snprintf(inner_color_full, sizeof(inner_color_full), "%s %s", adj, color);
                 int inner_id = get_bag_id(inner_color_full);
                 add_contained_bag(outer_id, inner_id, count);
            }
        }
    }
    fclose(file);

    if (shiny_gold_id == -1) {
         fprintf(stderr, "Shiny gold bag not found in input.\n");
         // Free memory before exiting
         for (int i = 0; i < bag_count; ++i) {
            free(rules[i].color_name);
            free(rules[i].contains);
         }
         return 1;
    }


    // Calculate Part 1
    int result_part1 = 0;
    for (int i = 0; i < bag_count; ++i) {
        // Reset memoization just in case (though only needed if calling multiple times with different targets)
        // For this problem structure, resetting isn't strictly needed after parsing
        // rules[i].can_contain_gold_memo = -1;
        if (i != shiny_gold_id) {
            // Clear memoization state before recalculating - important!
             for(int j=0; j < bag_count; ++j) rules[j].can_contain_gold_memo = -1;

            if (can_contain_shiny_gold_recursive(i)) {
                result_part1++;
            }
        }
    }


    // Calculate Part 2
    long long result_part2 = count_individual_bags_recursive(shiny_gold_id);

    printf("%d\n", result_part1);
    printf("%lld\n", result_part2);

    // Cleanup dynamically allocated memory
    for (int i = 0; i < bag_count; ++i) {
        free(rules[i].color_name);
        free(rules[i].contains);
    }

    return 0;
}

