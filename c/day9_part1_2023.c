
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 1024
#define MAX_NUMBERS 100

int allZeros(int nums[], int size) {
    for (int i = 0; i < size; i++) {
        if (nums[i] != 0) {
            return 0;
        }
    }
    return 1;
}

int calculateExtrapolation(int history[], int size, int extrapolations[]) {
    int newSize = 0;
    for (int i = 1; i < size; i++) {
        extrapolations[i-1] = history[i] - history[i-1];
        newSize++;
    }
    return newSize;
}

int solve(FILE* file) {
    char line[MAX_LINE_LENGTH];
    int res = 0;

    while (fgets(line, sizeof(line), file)) {
        int history[MAX_NUMBERS], historySize = 0;
        char* token = strtok(line, " \n");
        while (token != NULL) {
            history[historySize++] = atoi(token);
            token = strtok(NULL, " \n");
        }

        int extrapolationsSeries[MAX_NUMBERS][MAX_NUMBERS];
        int sizes[MAX_NUMBERS], levels = 1;
        memcpy(extrapolationsSeries[0], history, historySize * sizeof(int));
        sizes[0] = historySize;

        for (int i = 1; i < historySize; i++) {
            if (allZeros(extrapolationsSeries[i-1], sizes[i-1])) break;
            sizes[i] = calculateExtrapolation(extrapolationsSeries[i-1], sizes[i-1], extrapolationsSeries[i]);
            levels++;
        }

        int futurePrediction = 0;
        for (int i = levels - 1; i >= 0; i--) {
            futurePrediction += extrapolationsSeries[i][sizes[i]-1];
        }

        res += futurePrediction;
    }

    return res;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    printf("%d\n", solve(file));
    fclose(file);
    return 0;
}
