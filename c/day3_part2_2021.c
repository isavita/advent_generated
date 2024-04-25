#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int countBits(char **values, int bitIndex, int *zeros, int *ones) {
    *zeros = 0;
    *ones = 0;
    for (int i = 0; i < values[0][0]; i++) {
        if (values[i][bitIndex] == '0') {
            (*zeros)++;
        } else {
            (*ones)++;
        }
    }
}

char filterValues(char **values, int (*criteria)(int, int), int size) {
    int bitIndex = 0;
    while (size > 1) {
        int zeros, ones;
        countBits(values, bitIndex, &zeros, &ones);
        char keep = criteria(zeros, ones);
        values = filterByBit(values, bitIndex, keep, &size);
        bitIndex++;
    }
    return values[0][0];
}

char **filterByBit(char **values, int bitIndex, char keep, int *size) {
    char **filtered = (char **)malloc((*size) * sizeof(char *));
    int j = 0;
    for (int i = 0; i < *size; i++) {
        if (values[i][bitIndex] == keep) {
            filtered[j++] = values[i];
        }
    }
    *size = j;
    return filtered;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    char line[13];
    int size = 0;
    char **values = (char **)malloc(1000 * sizeof(char *));
    while (fgets(line, 13, file)) {
        values[size] = (char *)malloc(13 * sizeof(char));
        sscanf(line, "%s", values[size]);
        size++;
    }

    fclose(file);

    int oxygenGeneratorRating = strtol(filterValues(values, oxygenCriteria, size), NULL, 2);
    int co2ScrubberRating = strtol(filterValues(values, co2Criteria, size), NULL, 2);

    printf("%d\n", oxygenGeneratorRating * co2ScrubberRating);

    for (int i = 0; i < size; i++) {
        free(values[i]);
    }
    free(values);
    return 0;
}

int oxygenCriteria(int zeros, int ones) {
    return zeros > ones ? '0' : '1';
}

int co2Criteria(int zeros, int ones) {
    return zeros <= ones ? '0' : '1';
}