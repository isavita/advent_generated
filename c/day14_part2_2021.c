
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_RULES 100
#define MAX_TEMPLATE_LENGTH 10000

typedef struct {
    char pair[3];
    char insert;
} Rule;

void readInput(const char* filename, char* template, Rule* rules, int* ruleCount) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    fscanf(file, "%s\n", template);

    char pair[3], insert[2];
    *ruleCount = 0;
    while (fscanf(file, "%s -> %s\n", pair, insert) != EOF) {
        strcpy(rules[*ruleCount].pair, pair);
        rules[*ruleCount].insert = insert[0];
        (*ruleCount)++;
    }

    fclose(file);
}

int main() {
    char template[MAX_TEMPLATE_LENGTH];
    Rule rules[MAX_RULES];
    int ruleCount;

    readInput("input.txt", template, rules, &ruleCount);

    long long pairCounts[26][26] = {0};
    long long newPairCounts[26][26];
    int len = strlen(template);
    for (int i = 0; i < len - 1; i++) {
        pairCounts[template[i]-'A'][template[i+1]-'A']++;
    }

    for (int step = 0; step < 40; step++) {
        memset(newPairCounts, 0, sizeof(newPairCounts));
        for (int i = 0; i < ruleCount; i++) {
            int first = rules[i].pair[0] - 'A';
            int second = rules[i].pair[1] - 'A';
            int insert = rules[i].insert - 'A';
            long long count = pairCounts[first][second];
            newPairCounts[first][insert] += count;
            newPairCounts[insert][second] += count;
        }
        memcpy(pairCounts, newPairCounts, sizeof(pairCounts));
    }

    long long elementCounts[26] = {0};
    for (int i = 0; i < 26; i++) {
        for (int j = 0; j < 26; j++) {
            elementCounts[i] += pairCounts[i][j];
        }
    }
    elementCounts[template[len-1]-'A']++;

    long long maxCount = 0, minCount = LLONG_MAX;
    for (int i = 0; i < 26; i++) {
        if (elementCounts[i] > maxCount) maxCount = elementCounts[i];
        if (elementCounts[i] > 0 && elementCounts[i] < minCount) minCount = elementCounts[i];
    }

    printf("%lld\n", maxCount - minCount);

    return 0;
}
