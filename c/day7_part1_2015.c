
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MAX_LINES 1000
#define MAX_LINE_LENGTH 100
#define MAX_WIRE_NAME_LENGTH 10
#define MAX_ENTRIES 1000

typedef struct {
    char wire[MAX_WIRE_NAME_LENGTH];
    char rule[MAX_LINE_LENGTH];
} WireRule;

typedef struct {
    char key[MAX_WIRE_NAME_LENGTH];
    uint16_t value;
} MemoEntry;

int find_wire_index(WireRule *wire_rules, int num_rules, const char *wire) {
    for (int i = 0; i < num_rules; i++) {
        if (strcmp(wire_rules[i].wire, wire) == 0) {
            return i;
        }
    }
    return -1;
}

int find_memo_index(MemoEntry *memo, int num_entries, const char *key) {
    for (int i = 0; i < num_entries; i++) {
        if (strcmp(memo[i].key, key) == 0) {
            return i;
        }
    }
    return -1;
}

uint16_t memo_dfs(WireRule *wire_rules, int num_rules, const char *entry, MemoEntry *memo, int *memo_count) {
    int memo_index = find_memo_index(memo, *memo_count, entry);
    if (memo_index != -1) {
        return memo[memo_index].value;
    }

    uint16_t result;
    if (entry[0] >= '0' && entry[0] <= '9') {
        result = atoi(entry);
    } else {
        int wire_index = find_wire_index(wire_rules, num_rules, entry);
        char *rule = wire_rules[wire_index].rule;
        char part1[MAX_WIRE_NAME_LENGTH] = {0};
        char op[MAX_WIRE_NAME_LENGTH] = {0};
        char part2[MAX_WIRE_NAME_LENGTH] = {0};
        int num_parts = sscanf(rule, "%s %s %s", part1, op, part2);

        if (num_parts == 1) {
            result = memo_dfs(wire_rules, num_rules, part1, memo, memo_count);
        } else if (strcmp(part1, "NOT") == 0) {
            result = ~memo_dfs(wire_rules, num_rules, op, memo, memo_count);
        } else if (strcmp(op, "AND") == 0) {
            result = memo_dfs(wire_rules, num_rules, part1, memo, memo_count) & memo_dfs(wire_rules, num_rules, part2, memo, memo_count);
        } else if (strcmp(op, "OR") == 0) {
            result = memo_dfs(wire_rules, num_rules, part1, memo, memo_count) | memo_dfs(wire_rules, num_rules, part2, memo, memo_count);
        } else if (strcmp(op, "LSHIFT") == 0) {
            result = memo_dfs(wire_rules, num_rules, part1, memo, memo_count) << memo_dfs(wire_rules, num_rules, part2, memo, memo_count);
        } else if (strcmp(op, "RSHIFT") == 0) {
            result = memo_dfs(wire_rules, num_rules, part1, memo, memo_count) >> memo_dfs(wire_rules, num_rules, part2, memo, memo_count);
        }
    }

    strcpy(memo[*memo_count].key, entry);
    memo[*memo_count].value = result;
    (*memo_count)++;
    return result;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    WireRule wire_rules[MAX_LINES];
    int num_rules = 0;
    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file) != NULL) {
        char wire[MAX_WIRE_NAME_LENGTH];
        char rule[MAX_LINE_LENGTH];
        sscanf(line, "%[a-zA-Z0-9 ] -> %s", rule, wire);
        strcpy(wire_rules[num_rules].wire, wire);
        strcpy(wire_rules[num_rules].rule, rule);
        num_rules++;
    }
    fclose(file);

    MemoEntry memo[MAX_ENTRIES];
    int memo_count = 0;
    printf("%u\n", memo_dfs(wire_rules, num_rules, "a", memo, &memo_count));

    return 0;
}
