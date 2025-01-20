
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>

#define MAX_LINE_LENGTH 100
#define MAX_WIRES 500

typedef struct {
    char wire[10];
    char rule[50];
} WireRule;

int find_wire_index(const WireRule *wires, int num_wires, const char *wire) {
    for (int i = 0; i < num_wires; i++) {
        if (strcmp(wires[i].wire, wire) == 0) {
            return i;
        }
    }
    return -1;
}

uint16_t memo_dfs(const WireRule *wires, int num_wires, const char *entry, uint16_t *memo, int *memo_set) {
    int index = find_wire_index(wires, num_wires, entry);
    
    if (isdigit(entry[0])) {
        return (uint16_t)atoi(entry);
    }
    
    if (index != -1 && memo_set[index]) {
        return memo[index];
    }

    char rule_copy[50];
    strcpy(rule_copy, wires[index].rule);

    char *parts[3] = {NULL};
    parts[0] = strtok(rule_copy, " ");
    parts[1] = strtok(NULL, " ");
    parts[2] = strtok(NULL, " ");
    
    uint16_t result;

    if (parts[1] == NULL) {
        result = memo_dfs(wires, num_wires, parts[0], memo, memo_set);
    } else if (strcmp(parts[0], "NOT") == 0) {
        result = ~memo_dfs(wires, num_wires, parts[1], memo, memo_set);
    } else if (strcmp(parts[1], "AND") == 0) {
        result = memo_dfs(wires, num_wires, parts[0], memo, memo_set) & memo_dfs(wires, num_wires, parts[2], memo, memo_set);
    } else if (strcmp(parts[1], "OR") == 0) {
        result = memo_dfs(wires, num_wires, parts[0], memo, memo_set) | memo_dfs(wires, num_wires, parts[2], memo, memo_set);
    } else if (strcmp(parts[1], "LSHIFT") == 0) {
        result = memo_dfs(wires, num_wires, parts[0], memo, memo_set) << memo_dfs(wires, num_wires, parts[2], memo, memo_set);
    } else if (strcmp(parts[1], "RSHIFT") == 0) {
        result = memo_dfs(wires, num_wires, parts[0], memo, memo_set) >> memo_dfs(wires, num_wires, parts[2], memo, memo_set);
    }

    if(index != -1)
    {
        memo[index] = result;
        memo_set[index] = 1;
    }

    return result;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    WireRule wires[MAX_WIRES];
    int num_wires = 0;
    char line[MAX_LINE_LENGTH];

    while (fgets(line, sizeof(line), file) != NULL) {
        line[strcspn(line, "\n")] = 0;
        char wire[10], rule[50];
        sscanf(line, "%[a-zA-Z0-9 ] -> %s", rule, wire);
        strcpy(wires[num_wires].wire, wire);
        strcpy(wires[num_wires].rule, rule);
        num_wires++;
    }
    fclose(file);

    uint16_t memo[MAX_WIRES];
    int memo_set[MAX_WIRES];
    memset(memo_set, 0, sizeof(memo_set));

    uint16_t a_signal = memo_dfs(wires, num_wires, "a", memo, memo_set);
    
    int b_index = find_wire_index(wires, num_wires, "b");
    
    if (b_index != -1)
    {
        sprintf(wires[b_index].rule, "%u", a_signal);
    }
    
    memset(memo_set, 0, sizeof(memo_set));
    
    printf("%u\n", memo_dfs(wires, num_wires, "a", memo, memo_set));

    return 0;
}
