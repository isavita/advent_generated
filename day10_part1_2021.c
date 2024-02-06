
#include <stdio.h>
#include <stdlib.h>

#define MAX_LINE_LENGTH 1024

int checkLine(char *line, int *score);

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    int totalScore = 0;

    while (fgets(line, MAX_LINE_LENGTH, file) != NULL) {
        int score = 0;
        if (checkLine(line, &score)) {
            totalScore += score;
        }
    }

    if (ferror(file)) {
        perror("Error reading file");
        fclose(file);
        return 1;
    }

    fclose(file);

    printf("%d\n", totalScore);
    return 0;
}

int checkLine(char *line, int *score) {
    char stack[MAX_LINE_LENGTH];
    int stackTop = -1;
    *score = 0;

    for (int i = 0; line[i] != '\0' && line[i] != '\n'; i++) {
        switch (line[i]) {
            case '(':
            case '[':
            case '{':
            case '<':
                stack[++stackTop] = line[i];
                break;
            case ')':
            case ']':
            case '}':
            case '>':
                if (stackTop == -1) {
                    // Corrupted line, stack is empty
                    return 0;
                }
                char expected = (line[i] == ')') ? '(' : 
                                (line[i] == ']') ? '[' : 
                                (line[i] == '}') ? '{' : '<';
                if (stack[stackTop] != expected) {
                    // Corrupted line, wrong closing character
                    *score = (line[i] == ')') ? 3 : 
                             (line[i] == ']') ? 57 : 
                             (line[i] == '}') ? 1197 : 25137;
                    return 1;
                }
                stackTop--; // Pop from stack
                break;
        }
    }

    return 0; // Line is not corrupted
}
