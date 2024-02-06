
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char name[50];
    int ranges[2][2];
} Rule;

int isValid(int value, Rule rule) {
    for (int i = 0; i < 2; i++) {
        if (value >= rule.ranges[i][0] && value <= rule.ranges[i][1]) {
            return 1;
        }
    }
    return 0;
}

int toInt(char *s) {
    return atoi(s);
}

int isValidForAnyRule(int value, Rule *rules, int numRules) {
    for (int i = 0; i < numRules; i++) {
        if (isValid(value, rules[i])) {
            return 1;
        }
    }
    return 0;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    Rule rules[100];
    int numRules = 0;
    int scanningRules = 1;
    int errorRate = 0;

    char line[100];
    while (fgets(line, sizeof(line), file)) {
        if (strcmp(line, "\n") == 0) {
            continue;
        }
        if (strstr(line, "your ticket:") || strstr(line, "nearby tickets:")) {
            scanningRules = 0;
            continue;
        }
        if (scanningRules) {
            char name[50];
            int range1[2], range2[2];
            if (sscanf(line, "%[^:]: %d-%d or %d-%d", name, &range1[0], &range1[1], &range2[0], &range2[1]) == 5) {
                strcpy(rules[numRules].name, name);
                rules[numRules].ranges[0][0] = range1[0];
                rules[numRules].ranges[0][1] = range1[1];
                rules[numRules].ranges[1][0] = range2[0];
                rules[numRules].ranges[1][1] = range2[1];
                numRules++;
            }
        } else {
            char *token = strtok(line, ",");
            while (token != NULL) {
                int val = toInt(token);
                if (!isValidForAnyRule(val, rules, numRules)) {
                    errorRate += val;
                }
                token = strtok(NULL, ",");
            }
        }
    }

    printf("%d\n", errorRate);

    fclose(file);
    return 0;
}
