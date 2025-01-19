
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <ctype.h>
#include <math.h>

bool isSafeReport(int *levels, int len);

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
        int *levels = NULL;
        int levelCount = 0;
        char *token = line;
        int num;
        
        while(*token){
          if(isdigit(*token) || (*token == '-' && isdigit(*(token+1)))){
            num = strtol(token, &token, 10);
            levelCount++;
            levels = (int*)realloc(levels, sizeof(int) * levelCount);
            if(levels == NULL){
                perror("Memory allocation failed");
                return 1;
            }
            levels[levelCount-1] = num;
          } else {
            token++;
          }
        }

        if (isSafeReport(levels, levelCount)) {
            safeReportCount++;
        }
        free(levels);
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
