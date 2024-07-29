#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char* react(char* polymer) {
    int len = strlen(polymer);
    char* stack = malloc(len + 1);
    int stackIndex = 0;

    for (int i = 0; i < len; i++) {
        if (stackIndex > 0 && tolower(stack[stackIndex - 1]) == tolower(polymer[i]) && 
            stack[stackIndex - 1] != polymer[i]) {
            stackIndex--; // Remove the last character
        } else {
            stack[stackIndex++] = polymer[i]; // Add the current character to the stack
        }
    }

    stack[stackIndex] = '\0'; // Null-terminate the resulting polymer
    free(polymer); // Free the original polymer
    return stack; // Return the new polymer
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    char* polymer = NULL;
    size_t len = 0;
    getline(&polymer, &len, file);
    fclose(file);

    // Remove newline character if present
    size_t polymer_length = strlen(polymer);
    if (polymer[polymer_length - 1] == '\n') {
        polymer[polymer_length - 1] = '\0';
    }

    char* result = react(polymer);
    printf("%lu\n", strlen(result));
    free(result); // Free the final polymer
    return 0;
}