
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define INPUT_REPEAT 10000
#define PHASES 100

void repeatInput(const char *input, int *output, int times) {
    int len = strlen(input);
    for (int t = 0; t < times; t++) {
        for (int i = 0; i < len; i++) {
            output[t * len + i] = input[i] - '0';
        }
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    char input[65536]; // Adjust size according to the expected input length
    if (fgets(input, sizeof(input), file) == NULL) {
        fclose(file);
        perror("Error reading file");
        return EXIT_FAILURE;
    }
    fclose(file);

    int len = strlen(input);
    if (input[len - 1] == '\n') {
        input[len - 1] = '\0'; // Remove newline character
        len--;
    }

    int offset = atoi(strndup(input, 7));
    int *repeatedInput = (int *)malloc(len * INPUT_REPEAT * sizeof(int));
    if (repeatedInput == NULL) {
        perror("Memory allocation failed");
        return EXIT_FAILURE;
    }

    repeatInput(input, repeatedInput, INPUT_REPEAT);

    for (int phase = 0; phase < PHASES; phase++) {
        int sum = 0;
        for (int i = len * INPUT_REPEAT - 1; i >= offset; i--) {
            sum += repeatedInput[i];
            repeatedInput[i] = sum % 10;
        }
    }

    for (int i = offset; i < offset + 8; i++) {
        printf("%d", repeatedInput[i]);
    }
    printf("\n");

    free(repeatedInput);

    return EXIT_SUCCESS;
}
