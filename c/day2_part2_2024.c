
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <string.h>

bool isSafeReport(int *levels, int len);
bool isSafeWithOneRemoval(int *levels, int len);

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Failed to open input file");
        return 1;
    }

    int safeReportCount = 0;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    while ((read = getline(&line, &len, file)) != -1) {
        int levels[100];
        int levelCount = 0;
        char *token = strtok(line, " \n");
        while (token != NULL) {
            levels[levelCount++] = atoi(token);
            token = strtok(NULL, " \n");
        }

        if (isSafeReport(levels, levelCount) || isSafeWithOneRemoval(levels, levelCount)) {
            safeReportCount++;
        }
    }

    free(line);
    fclose(file);
    printf("%d\n", safeReportCount);
    return 0;
}


bool isSafeReport(int *levels, int len) {
    if (len < 2) {
        return false;
    }

    int firstDiff = levels[1] - levels[0];
    if (firstDiff == 0) {
        return false;
    }

    bool isIncreasing = firstDiff > 0;

    for (int i = 0; i < len - 1; i++) {
        int diff = levels[i + 1] - levels[i];

        if (diff == 0) {
            return false;
        }

        if ((isIncreasing && diff <= 0) || (!isIncreasing && diff >= 0)) {
            return false;
        }

        int absDiff = abs(diff);
        if (absDiff < 1 || absDiff > 3) {
            return false;
        }
    }

    return true;
}


bool isSafeWithOneRemoval(int *levels, int len) {
    if (len <= 2) {
        return false;
    }
     
    int modifiedLevels[100];
    for (int i = 0; i < len; i++) {
         int modifiedLen = 0;
         for (int j = 0; j < len; j++) {
            if(i!=j)
               modifiedLevels[modifiedLen++] = levels[j];
         }
        if (isSafeReport(modifiedLevels, modifiedLen)) {
            return true;
        }
    }
    return false;
}
