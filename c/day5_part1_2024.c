
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LEN 2048
#define INITIAL_CAPACITY 16
#define MAX_UPDATE_LEN 256
#define MAX_POS_VALUE 65535 // Optimization assumption for lookup array

typedef struct {
    int x;
    int y;
} Rule;

// Optimization: Use a lookup array for faster position checks in is_correct
int pos_lookup[MAX_POS_VALUE + 1];

bool is_correct(int* update, int update_size, Rule* rules, int num_rules) {
    // Initialize lookup array efficiently
    memset(pos_lookup, -1, sizeof(pos_lookup));

    // Populate lookup array: map value to its position
    for (int i = 0; i < update_size; ++i) {
        // Ensure value is within the bounds of the lookup array
        if (update[i] >= 0 && update[i] <= MAX_POS_VALUE) {
            pos_lookup[update[i]] = i;
        } else {
            // Handle or ignore values outside the assumed range if necessary.
            // For this problem, we might assume valid inputs or need dynamic resizing/hashing
            // if MAX_POS_VALUE is too limiting or unknown. Sticking to static for optimization.
        }
    }

    // Check rules using the lookup array
    for (int i = 0; i < num_rules; ++i) {
        int x = rules[i].x;
        int y = rules[i].y;
        int pos_x = -1;
        int pos_y = -1;

        // Check if x and y are within lookup bounds before accessing
        if (x >= 0 && x <= MAX_POS_VALUE) pos_x = pos_lookup[x];
        if (y >= 0 && y <= MAX_POS_VALUE) pos_y = pos_lookup[y];

        // If both x and y exist in the update, check their order
        if (pos_x != -1 && pos_y != -1 && pos_x > pos_y) {
            return false; // Rule violated
        }
    }
    return true; // All rules satisfied
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        return 1;
    }

    Rule *rules = NULL;
    int num_rules = 0;
    int capacity_rules = 0;

    int **updates = NULL;
    int *update_sizes = NULL;
    int num_updates = 0;
    int capacity_updates = 0;

    char line[MAX_LINE_LEN];

    // Parse rules first
    while (fgets(line, sizeof(line), fp)) {
        line[strcspn(line, "\r\n")] = 0; // Remove trailing newline/CR
        if (strlen(line) == 0) continue; // Skip empty lines

        if (strchr(line, '|')) {
            if (num_rules >= capacity_rules) {
                capacity_rules = (capacity_rules == 0) ? INITIAL_CAPACITY : capacity_rules * 2;
                Rule* new_rules = realloc(rules, capacity_rules * sizeof(Rule));
                if (!new_rules) { perror("realloc rules failed"); exit(1); }
                rules = new_rules;
            }
            if (sscanf(line, "%d|%d", &rules[num_rules].x, &rules[num_rules].y) == 2) {
                num_rules++;
            }
        } else {
            // First line without '|' marks the end of rules, rewind slightly
            // or handle the first update line here. Simpler to handle here.
            goto parse_updates;
        }
    }

parse_updates:
    // Process the line already read if it was the first update line
    if (strlen(line) > 0 && !strchr(line, '|')) {
         if (num_updates >= capacity_updates) {
            capacity_updates = (capacity_updates == 0) ? INITIAL_CAPACITY : capacity_updates * 2;
            int** new_updates = realloc(updates, capacity_updates * sizeof(int*));
            int* new_sizes = realloc(update_sizes, capacity_updates * sizeof(int));
             if (!new_updates || !new_sizes) { perror("realloc updates failed"); exit(1); }
             updates = new_updates;
             update_sizes = new_sizes;
         }

        int current_update[MAX_UPDATE_LEN]; // Temp buffer for current update ints
        int current_size = 0;
        char *token = strtok(line, ",");
        while (token != NULL && current_size < MAX_UPDATE_LEN) {
            current_update[current_size++] = atoi(token);
            token = strtok(NULL, ",");
        }

        updates[num_updates] = malloc(current_size * sizeof(int));
         if (!updates[num_updates]) { perror("malloc update failed"); exit(1); }
        memcpy(updates[num_updates], current_update, current_size * sizeof(int));
        update_sizes[num_updates] = current_size;
        num_updates++;
    }


    // Parse remaining updates
    while (fgets(line, sizeof(line), fp)) {
        line[strcspn(line, "\r\n")] = 0;
        if (strlen(line) == 0) continue;

         if (num_updates >= capacity_updates) {
            capacity_updates = (capacity_updates == 0) ? INITIAL_CAPACITY : capacity_updates * 2;
            int** new_updates = realloc(updates, capacity_updates * sizeof(int*));
            int* new_sizes = realloc(update_sizes, capacity_updates * sizeof(int));
             if (!new_updates || !new_sizes) { perror("realloc updates failed"); exit(1); }
             updates = new_updates;
             update_sizes = new_sizes;
         }

        int current_update[MAX_UPDATE_LEN];
        int current_size = 0;
        char *token = strtok(line, ",");
         while (token != NULL && current_size < MAX_UPDATE_LEN) {
            current_update[current_size++] = atoi(token);
            token = strtok(NULL, ",");
        }

        updates[num_updates] = malloc(current_size * sizeof(int));
         if (!updates[num_updates]) { perror("malloc update failed"); exit(1); }
        memcpy(updates[num_updates], current_update, current_size * sizeof(int));
        update_sizes[num_updates] = current_size;
        num_updates++;
    }
    fclose(fp);

    long long total_sum = 0;
    for (int i = 0; i < num_updates; ++i) {
        if (update_sizes[i] > 0 && is_correct(updates[i], update_sizes[i], rules, num_rules)) {
             // Ensure index is valid before accessing
             int mid_index = update_sizes[i] / 2;
             total_sum += updates[i][mid_index];
        }
    }

    printf("%lld\n", total_sum);

    // Free allocated memory
    free(rules);
    for (int i = 0; i < num_updates; ++i) {
        free(updates[i]);
    }
    free(updates);
    free(update_sizes);

    return 0;
}
