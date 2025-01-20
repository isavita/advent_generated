
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_RULES 100
#define MAX_POLYMER 200000

typedef struct {
    char pair[3];
    char insert;
} Rule;

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening file");
        return 1;
    }

    char polymer[MAX_POLYMER];
    fgets(polymer, MAX_POLYMER, fp);
    polymer[strcspn(polymer, "\n")] = 0;

    Rule rules[MAX_RULES];
    int numRules = 0;
    char line[10];
    while (fgets(line, sizeof(line), fp)) {
        if (strlen(line) <= 1) continue;
        sscanf(line, "%2s -> %c", rules[numRules].pair, &rules[numRules].insert);
        numRules++;
    }
    fclose(fp);

    for (int step = 0; step < 10; step++) {
      char newPolymer[MAX_POLYMER] = {0};
      int newPolymerLen = 0;
        for (int i = 0; polymer[i+1] != 0 ; i++) {
            newPolymer[newPolymerLen++] = polymer[i];
            for (int j = 0; j < numRules; j++) {
                if (polymer[i] == rules[j].pair[0] && polymer[i+1] == rules[j].pair[1]) {
                    newPolymer[newPolymerLen++] = rules[j].insert;
                    break;
                }
            }
        }
        newPolymer[newPolymerLen++] = polymer[strlen(polymer)-1];
        strcpy(polymer, newPolymer);
    }

  int counts[256] = {0};
  for (int i = 0; polymer[i] != 0 ; i++)
  {
      counts[(unsigned char)polymer[i]]++;
  }
    int min = INT_MAX, max = 0;
    for (int i = 0; i < 256; i++) {
        if (counts[i] > 0) {
            if (counts[i] < min) min = counts[i];
            if (counts[i] > max) max = counts[i];
        }
    }
    printf("%d\n", max - min);
    return 0;
}
