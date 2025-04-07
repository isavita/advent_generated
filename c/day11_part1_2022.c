
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LEN 256
#define INITIAL_ITEM_CAPACITY 16
#define INITIAL_MONKEY_CAPACITY 8

typedef enum {
    OP_ADD,
    OP_MULTIPLY,
    OP_SQUARE
} OpType;

typedef struct {
    unsigned long long *items;
    int item_count;
    int item_capacity;

    OpType op_type;
    int op_value; // Used if op_type is ADD or MULTIPLY

    int test_div;
    int true_monkey;
    int false_monkey;

    unsigned long long inspections;
} Monkey;

// Function to add an item to a monkey's list (dynamic array)
void add_item(Monkey *m, unsigned long long item) {
    if (m->item_count >= m->item_capacity) {
        m->item_capacity = (m->item_capacity == 0) ? INITIAL_ITEM_CAPACITY : m->item_capacity * 2;
        m->items = (unsigned long long *)realloc(m->items, m->item_capacity * sizeof(unsigned long long));
        if (m->items == NULL) {
            perror("Failed to reallocate items array");
            exit(EXIT_FAILURE);
        }
    }
    m->items[m->item_count++] = item;
}

// Comparison function for qsort (descending order)
int compare_ull_desc(const void *a, const void *b) {
    unsigned long long val_a = *(const unsigned long long *)a;
    unsigned long long val_b = *(const unsigned long long *)b;
    if (val_a < val_b) return 1;
    if (val_a > val_b) return -1;
    return 0;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening input.txt");
        return EXIT_FAILURE;
    }

    Monkey *monkeys = NULL;
    int monkey_count = 0;
    int monkey_capacity = 0;
    char line[MAX_LINE_LEN];

    // Parse Monkeys
    while (fgets(line, sizeof(line), fp)) { // Reads "Monkey N:"
        if (strncmp(line, "Monkey", 6) != 0) continue; // Skip blank lines between monkeys

        if (monkey_count >= monkey_capacity) {
            monkey_capacity = (monkey_capacity == 0) ? INITIAL_MONKEY_CAPACITY : monkey_capacity * 2;
            monkeys = (Monkey *)realloc(monkeys, monkey_capacity * sizeof(Monkey));
            if (monkeys == NULL) {
                perror("Failed to reallocate monkeys array");
                fclose(fp);
                exit(EXIT_FAILURE);
            }
        }
        Monkey *m = &monkeys[monkey_count];
        memset(m, 0, sizeof(Monkey)); // Initialize monkey struct

        // Parse Starting items
        if (!fgets(line, sizeof(line), fp)) break; // EOF
        char *items_ptr = strstr(line, ": ");
        if (items_ptr) {
            items_ptr += 2; // Move past ": "
            char *token = strtok(items_ptr, ", \n");
            while (token != NULL) {
                add_item(m, strtoull(token, NULL, 10));
                token = strtok(NULL, ", \n");
            }
        }

        // Parse Operation
        if (!fgets(line, sizeof(line), fp)) break; // EOF
        char *op_ptr = strstr(line, "old ");
        if (op_ptr) {
            op_ptr += 4; // Move past "old "
            if (*op_ptr == '+') {
                m->op_type = OP_ADD;
                if (sscanf(op_ptr + 2, "%d", &m->op_value) != 1) {
                     // Check if it's "old + old"
                    if (strstr(op_ptr + 2, "old")) {
                         m->op_type = OP_SQUARE; // Treat "old + old" as "old * 2", but SQUARE handles "old * old"
                         // Correction: Python code handles "old + old" specifically. Let's add OP_DOUBLE
                         // Re-thinking: The Python handles old + old and old * old.
                         // Let's stick to ADD/MULTIPLY/SQUARE. old + old means op_type=ADD, op_value means add itself. Let's make op_value = -1 mean "use old".
                         // Simpler: Python code translates 'old + old' to lambda old: old + old.
                         // Python code translates 'old * old' to lambda old: old * old. Let's use SQUARE for old * old.
                         // And let's use OP_ADD with a special value (e.g., 0 or -1) to indicate adding 'old'.
                         // For simplicity, let's stick to the provided Python logic:
                         // If "+ old", it's old + old -> change op_type to OP_MULTIPLY, op_value = 2 ? No, that's wrong.
                         // Let's make it simple: ADD/MULTIPLY handle constant operands. SQUARE handles 'old * old'. Let's add OP_DOUBLE for 'old + old'.
                         // Re-re-thinking: The provided Python code doesn't actually have OP_DOUBLE, it has `lambda old: old + old`.
                         // Let's refine the C struct:
                         // OpType: ADD_VAL, MULT_VAL, ADD_OLD, MULT_OLD
                         // No, keep it simple: ADD, MULTIPLY, SQUARE. If operand is "old", handle it specially during calculation.
                         char *operand_str = op_ptr + 2;
                         while (isspace((unsigned char)*operand_str)) operand_str++;
                         if (strncmp(operand_str, "old", 3) == 0) {
                             // This case is tricky, Python uses lambda.
                             // Let's assume ADD/MULTIPLY always have a numeric value based on Python's structure for now.
                             // The python code specifically checks if f[2] == "old".
                             // Let's stick to ADD, MULTIPLY, SQUARE.
                             fprintf(stderr, "Parsing error: 'old + old' requires special handling not fully implemented.\n");
                             // Correction: The Python *does* handle it. lambda old: old + old.
                             // In C: op_type = OP_ADD, op_value = -1 (special flag for 'old') ?
                             // Let's refine the parse logic slightly.

                             // Let's revert to ADD, MULTIPLY, SQUARE and handle the 'old' case in the operation logic.
                             // The python code has specific lambdas.
                             // Let's parse fully:
                             char op_char = *op_ptr;
                             char operand[10];
                             sscanf(op_ptr + 2, "%s", operand);

                             if (strcmp(operand, "old") == 0) {
                                if (op_char == '*') m->op_type = OP_SQUARE;
                                else if (op_char == '+') {
                                     m->op_type = OP_ADD;
                                     m->op_value = -1; // Signal "add old"
                                }
                             } else {
                                m->op_value = atoi(operand);
                                if (op_char == '*') m->op_type = OP_MULTIPLY;
                                else if (op_char == '+') m->op_type = OP_ADD;
                             }


                        }
                    }
                }
            } else if (*op_ptr == '*') {
                 m->op_type = OP_MULTIPLY;
                 if (sscanf(op_ptr + 2, "%d", &m->op_value) != 1) {
                     // Check if it's "old * old"
                     if (strstr(op_ptr + 2, "old")) {
                         m->op_type = OP_SQUARE;
                     }
                 }
            }
        }


        // Parse Test
        if (!fgets(line, sizeof(line), fp)) break; // EOF
        sscanf(line, "  Test: divisible by %d", &m->test_div);

        // Parse True
        if (!fgets(line, sizeof(line), fp)) break; // EOF
        sscanf(line, "    If true: throw to monkey %d", &m->true_monkey);

        // Parse False
        if (!fgets(line, sizeof(line), fp)) break; // EOF
        sscanf(line, "    If false: throw to monkey %d", &m->false_monkey);

        monkey_count++;
    }
    fclose(fp);

    // Simulate Rounds
    int rounds = 20;
    for (int r = 0; r < rounds; ++r) {
        for (int i = 0; i < monkey_count; ++i) {
            Monkey *m = &monkeys[i];
            int items_to_process = m->item_count; // Process only items present at round start
             m->inspections += items_to_process;

            for (int j = 0; j < items_to_process; ++j) {
                // Get item (always from the front)
                unsigned long long item = m->items[0];

                // Apply operation
                unsigned long long current_val = item;
                switch (m->op_type) {
                    case OP_ADD:
                        // Handle the special "add old" case
                        item = (m->op_value == -1) ? current_val + current_val : current_val + m->op_value;
                        break;
                    case OP_MULTIPLY:
                        item = current_val * m->op_value;
                        break;
                    case OP_SQUARE:
                        item = current_val * current_val;
                        break;
                }

                // Monkey gets bored
                item /= 3;

                // Test and throw
                int target_monkey_idx = (item % m->test_div == 0) ? m->true_monkey : m->false_monkey;
                add_item(&monkeys[target_monkey_idx], item);

                // Remove item from current monkey (shift remaining items)
                m->item_count--;
                if (m->item_count > 0) {
                    // Shift items left efficiently using memmove
                    memmove(m->items, m->items + 1, m->item_count * sizeof(unsigned long long));
                }
                 // Optimization: Instead of shifting, use a circular buffer or just track start index.
                 // But for simplicity and given typical constraints, memmove is often acceptable here.
                 // Let's stick to the simple shift for now.
            }
             // After processing the initial items, clear the processed ones efficiently
            // If we used the shift method above, the items are already effectively removed.
            // If we track a start index 'm->item_start', we'd update it here: m->item_start += items_to_process;
            // Or simply reset the count if we process all items and copy them away:
             // m->item_count = 0; // Incorrect if new items were added *during* the turn (not possible in this problem)

            // The shift implementation above correctly handles removing items one by one.
        }
    }


    // Calculate Monkey Business
    unsigned long long *all_inspections = (unsigned long long *)malloc(monkey_count * sizeof(unsigned long long));
    if (all_inspections == NULL) {
        perror("Failed to allocate inspections array");
        // Cleanup monkeys before exiting
        for (int i = 0; i < monkey_count; ++i) {
            free(monkeys[i].items);
        }
        free(monkeys);
        return EXIT_FAILURE;
    }

    for (int i = 0; i < monkey_count; ++i) {
        all_inspections[i] = monkeys[i].inspections;
    }

    qsort(all_inspections, monkey_count, sizeof(unsigned long long), compare_ull_desc);

    unsigned long long monkey_business = 0;
    if (monkey_count >= 2) {
        monkey_business = all_inspections[0] * all_inspections[1];
    } else if (monkey_count == 1) {
        monkey_business = all_inspections[0];
    }

    printf("%llu\n", monkey_business);

    // Cleanup
    free(all_inspections);
    for (int i = 0; i < monkey_count; ++i) {
        free(monkeys[i].items);
    }
    free(monkeys);

    return EXIT_SUCCESS;
}
