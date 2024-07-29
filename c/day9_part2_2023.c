#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int* parseStringToInts(char* line, int* count) {
    int* numbers = NULL;
    char* token = strtok(line, " ");
    while (token) {
        numbers = realloc(numbers, (*count + 1) * sizeof(int));
        numbers[(*count)++] = atoi(token);
        token = strtok(NULL, " ");
    }
    return numbers;
}

int allZeros(int* nums, int count) {
    for (int i = 0; i < count; i++) {
        if (nums[i] != 0) return 0;
    }
    return 1;
}

int* calculateExtrapolation(int* history, int count, int* newCount) {
    int* extrapolations = malloc((count - 1) * sizeof(int));
    for (int i = 1; i < count; i++) {
        extrapolations[i - 1] = history[i] - history[i - 1];
    }
    *newCount = count - 1;
    return extrapolations;
}

int** calculateExtrapolations(int* history, int count, int* seriesCount) {
    int** extrapolationsSeries = malloc(count * sizeof(int*));
    extrapolationsSeries[0] = history;
    *seriesCount = 1;

    while (count > 1) {
        int newCount;
        int* extrapolations = calculateExtrapolation(history, count, &newCount);
        extrapolationsSeries[*seriesCount] = extrapolations;
        (*seriesCount)++;
        history = extrapolations;
        count = newCount;

        if (allZeros(history, count)) break;
    }
    return extrapolationsSeries;
}

int solve(char** input, int lineCount) {
    int res = 0;
    for (int i = 0; i < lineCount; i++) {
        int count = 0;
        int* history = parseStringToInts(input[i], &count);
        int seriesCount;
        int** extrapolationsSeries = calculateExtrapolations(history, count, &seriesCount);

        int pastPrediction = 0;
        for (int j = seriesCount - 1; j >= 0; j--) {
            pastPrediction = extrapolationsSeries[j][0] - pastPrediction;
        }

        res += pastPrediction;
        free(history);
        for (int j = 1; j < seriesCount; j++) free(extrapolationsSeries[j]);
        free(extrapolationsSeries);
    }
    return res;
}

char** readFile(const char* fileName, int* lineCount) {
    FILE* file = fopen(fileName, "r");
    if (!file) exit(1);
    
    char** lines = NULL;
    char buffer[256];
    *lineCount = 0;
    while (fgets(buffer, sizeof(buffer), file)) {
        lines = realloc(lines, (*lineCount + 1) * sizeof(char*));
        lines[*lineCount] = strdup(buffer);
        (*lineCount)++;
    }
    fclose(file);
    return lines;
}

int main() {
    int lineCount;
    char** input = readFile("input.txt", &lineCount);
    printf("%d\n", solve(input, lineCount));
    for (int i = 0; i < lineCount; i++) free(input[i]);
    free(input);
    return 0;
}