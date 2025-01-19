
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <stdbool.h>

int canSplit(int *packages, int packages_len, int firstGroupComb, int targetWeight);

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    int packages[100];
    int packages_len = 0;
    int totalWeight = 0;
    char line[20];

    while (fgets(line, sizeof(line), fp) != NULL) {
        packages[packages_len] = atoi(line);
        totalWeight += packages[packages_len];
        packages_len++;
    }
    fclose(fp);

    int targetWeight = totalWeight / 4;
    long long bestQE = LLONG_MAX;
    int bestLength = INT_MAX;

    for (int comb = 1; comb < (1 << packages_len); comb++) {
        int groupWeight = 0;
        long long qe = 1;
        int groupLength = 0;

        for (int i = 0; i < packages_len; i++) {
            if (comb & (1 << i)) {
                groupWeight += packages[i];
                qe *= packages[i];
                groupLength++;
            }
        }
        if (groupWeight == targetWeight && groupLength <= bestLength) {
            if (groupLength < bestLength || qe < bestQE) {
              if(canSplit(packages,packages_len,comb,targetWeight)){
                  bestLength = groupLength;
                  bestQE = qe;
              }
            }
        }
    }

    printf("%lld\n", bestQE);

    return 0;
}

int canSplit(int *packages, int packages_len, int firstGroupComb, int targetWeight) {
    int remainingPackages[100];
    int remainingPackages_len = 0;
    for (int i = 0; i < packages_len; i++) {
        if (!(firstGroupComb & (1 << i))) {
            remainingPackages[remainingPackages_len++] = packages[i];
        }
    }
    for (int comb1 = 1; comb1 < (1 << remainingPackages_len); comb1++) {
        int group1Weight = 0;
        for (int i = 0; i < remainingPackages_len; i++) {
            if (comb1 & (1 << i)) {
                group1Weight += remainingPackages[i];
            }
        }
        if (group1Weight == targetWeight) {
           for (int comb2 = 1; comb2 < (1 << remainingPackages_len); comb2++) {
              if (!(comb1 & comb2)) {
                int group2Weight = 0;
                for (int i = 0; i < remainingPackages_len; i++) {
                  if (comb2 & (1 << i)) {
                    group2Weight += remainingPackages[i];
                  }
                }
                 if (group2Weight == targetWeight) {
                    return 1;
                 }
              }
           }
        }
    }
    return 0;
}
