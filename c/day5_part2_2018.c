
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int react(char *polymer) {
    int reactionOccurred = 1;
    while (reactionOccurred) {
        reactionOccurred = 0;
        for (int i = 0; polymer[i] != '\0' && polymer[i+1] != '\0'; i++) {
            if (polymer[i] != polymer[i+1] && toupper(polymer[i]) == toupper(polymer[i+1])) {
                memmove(polymer + i, polymer + i + 2, strlen(polymer) - i - 1);
                reactionOccurred = 1;
            }
        }
    }
    return strlen(polymer);
}

void removeUnit(char *polymer, char unit, char *result) {
    int j = 0;
    for (int i = 0; polymer[i] != '\0'; i++) {
        if (tolower(polymer[i]) != tolower(unit)) {
            result[j++] = polymer[i];
        }
    }
    result[j] = '\0';
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error reading file\n");
        return 1;
    }
    char *polymer = malloc(50000); // Assuming input does not exceed 50,000 characters
    if (polymer == NULL) {
        printf("Memory allocation failed\n");
        fclose(file);
        return 1;
    }
    char *tempPolymer = malloc(50000);
    if (tempPolymer == NULL) {
        printf("Memory allocation failed\n");
        free(polymer);
        fclose(file);
        return 1;
    }
    fscanf(file, "%[^\n]", polymer);
    fclose(file);

    int minLength = strlen(polymer);
    for (char unit = 'a'; unit <= 'z'; unit++) {
        removeUnit(polymer, unit, tempPolymer);
        char *reactedPolymer = strdup(tempPolymer);
        if (reactedPolymer == NULL) {
            printf("Memory allocation failed\n");
            free(polymer);
            free(tempPolymer);
            return 1;
        }
        int length = react(reactedPolymer);
        if (length < minLength) {
            minLength = length;
        }
        free(reactedPolymer);
    }

    printf("%d\n", minLength);
    free(polymer);
    free(tempPolymer);
    return 0;
}
