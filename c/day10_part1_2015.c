
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* readInput(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        return NULL;
    }

    char* sequence = NULL;
    size_t len = 0;
    ssize_t read;

    if ((read = getline(&sequence, &len, file)) != -1) {
        sequence[strcspn(sequence, "\n")] = 0;
    }

    fclose(file);
    return sequence;
}

char* nextSequence(char* sequence) {
    char* result = (char*)malloc(strlen(sequence) * 2 + 1);
    int i = 0;

    while (i < strlen(sequence)) {
        int count = 1;
        char digit = sequence[i];
        int j = i + 1;

        while (j < strlen(sequence) && sequence[j] == digit) {
            count++;
            j++;
        }

        sprintf(result + strlen(result), "%d%c", count, digit);
        i += count;
    }

    return result;
}

char* lookAndSay(char* sequence, int iterations) {
    for (int i = 0; i < iterations; i++) {
        sequence = nextSequence(sequence);
    }
    return sequence;
}

int main() {
    char* initialSequence = readInput("input.txt");
    if (initialSequence == NULL) {
        printf("Error reading input\n");
        return 1;
    }

    char* result = lookAndSay(initialSequence, 40);
    printf("%ld\n", strlen(result));

    free(initialSequence);
    free(result);

    return 0;
}
