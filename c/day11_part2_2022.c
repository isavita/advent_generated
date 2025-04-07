
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_MONKEYS 10
#define MAX_ITEMS 1024 // Initial capacity, will realloc if needed
#define MAX_LINE_LEN 256
#define ROUNDS 10000

typedef enum { ADD, MULT } OpType;

typedef struct {
    long long *items;
    int num_items;
    int capacity;
    OpType op_type;
    long long op_val; // -1 represents "old"
    long long div;
    int next[2]; // [false_target, true_target] - Note: Python used [true, false] index order
    long long inspections;
} Monkey;

// Comparison function for qsort (descending order)
int compare_long_long_desc(const void *a, const void *b) {
    long long val_a = *(const long long *)a;
    long long val_b = *(const long long *)b;
    if (val_a < val_b) return 1;
    if (val_a > val_b) return -1;
    return 0;
}

void add_item(Monkey *m, long long item) {
    if (m->num_items >= m->capacity) {
        m->capacity = (m->capacity == 0) ? MAX_ITEMS : m->capacity * 2;
        long long *new_items = realloc(m->items, m->capacity * sizeof(long long));
        if (!new_items) {
            perror("Failed to reallocate items");
            exit(EXIT_FAILURE);
        }
        m->items = new_items;
    }
    m->items[m->num_items++] = item;
}

long long remove_first_item(Monkey *m) {
    if (m->num_items <= 0) {
        fprintf(stderr, "Error: Trying to remove item from empty list\n");
        exit(EXIT_FAILURE);
    }
    long long item = m->items[0];
    // Shift remaining items left
    if (m->num_items > 1) {
        memmove(&m->items[0], &m->items[1], (m->num_items - 1) * sizeof(long long));
    }
    m->num_items--;
    return item;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening file");
        return 1;
    }

    Monkey monkeys[MAX_MONKEYS];
    int num_monkeys = 0;
    char line[MAX_LINE_LEN];
    char *token;

    // Initialize monkeys
    for (int i = 0; i < MAX_MONKEYS; ++i) {
        monkeys[i].items = NULL;
        monkeys[i].num_items = 0;
        monkeys[i].capacity = 0;
        monkeys[i].inspections = 0;
    }

    // Parse input
    while (fgets(line, sizeof(line), fp)) { // Monkey ID line (ignore)
        if (num_monkeys >= MAX_MONKEYS) break;
        Monkey *m = &monkeys[num_monkeys];

        // Starting items
        fgets(line, sizeof(line), fp);
        char *items_str = strchr(line, ':') + 2;
        token = strtok(items_str, ", ");
        while (token != NULL) {
            add_item(m, strtoll(token, NULL, 10));
            token = strtok(NULL, ", ");
        }

        // Operation
        fgets(line, sizeof(line), fp);
        char op_char;
        char op_val_str[20];
        sscanf(line, "  Operation: new = old %c %s", &op_char, op_val_str);
        m->op_type = (op_char == '+') ? ADD : MULT;
        if (strcmp(op_val_str, "old") == 0) {
            m->op_val = -1;
        } else {
            m->op_val = strtoll(op_val_str, NULL, 10);
        }

        // Test
        fgets(line, sizeof(line), fp);
        sscanf(line, "  Test: divisible by %lld", &m->div);

        // True target
        fgets(line, sizeof(line), fp);
        sscanf(line, "    If true: throw to monkey %d", &m->next[1]); // index 1 for true

        // False target
        fgets(line, sizeof(line), fp);
        sscanf(line, "    If false: throw to monkey %d", &m->next[0]); // index 0 for false

        num_monkeys++;
        fgets(line, sizeof(line), fp); // Consume blank line
    }
    fclose(fp);

    // Calculate modulo base
    long long total_div = 1;
    for (int i = 0; i < num_monkeys; ++i) {
        total_div *= monkeys[i].div;
    }

    // Simulate rounds
    for (int round = 0; round < ROUNDS; ++round) {
        for (int i = 0; i < num_monkeys; ++i) {
            Monkey *m = &monkeys[i];
            while (m->num_items > 0) {
                m->inspections++;
                long long item = remove_first_item(m);
                long long operand = (m->op_val == -1) ? item : m->op_val;

                if (m->op_type == ADD) {
                    item += operand;
                } else { // MULT
                    item *= operand;
                }

                item %= total_div; // Worry management for part 2

                int target_monkey_idx = m->next[(item % m->div == 0)];
                add_item(&monkeys[target_monkey_idx], item);
            }
        }
    }

    // Calculate monkey business
    long long inspections[MAX_MONKEYS];
    for (int i = 0; i < num_monkeys; ++i) {
        inspections[i] = monkeys[i].inspections;
    }

    qsort(inspections, num_monkeys, sizeof(long long), compare_long_long_desc);

    long long monkey_business = inspections[0] * inspections[1];
    printf("%lld\n", monkey_business);

    // Free allocated memory
    for (int i = 0; i < num_monkeys; ++i) {
        free(monkeys[i].items);
    }

    return 0;
}
