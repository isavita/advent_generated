
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define hashTableSize 256

typedef struct {
    char* label;
    int numBox;
    char operation;
    int number;
} Step;

int hashString(char* str) {
    int res = 0;
    for (int i = 0; i < strlen(str); i++) {
        char character = str[i];
        res += (int)character;
        res *= 17;
        res %= hashTableSize;
    }
    return res;
}

Step parseStep(char* stepStr) {
    Step step;

    step.label = strdup(stepStr);
    char* labelEnd = strpbrk(step.label, "=-0123456789");
    *labelEnd = '\0';
    step.numBox = hashString(step.label);
    step.operation = stepStr[labelEnd - step.label];
    if (step.operation == '=') {
        step.number = atoi(labelEnd + 1);
    }

    return step;
}

int solve(char** input) {
    char* line = input[0];
    char* token = strtok(line, ",");
    int res = 0;
    while (token != NULL) {
        res += hashString(token);
        token = strtok(NULL, ",");
    }
    return res;
}

char** readFile(char* fileName) {
    FILE* file = fopen(fileName, "r");
    if (!file) {
        perror("Error opening file");
        exit(1);
    }
    
    // Determine file size
    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read file contents into buffer
    char* buffer = (char*)malloc(fileSize);
    fread(buffer, 1, fileSize, file);
    fclose(file);

    // Tokenize buffer by new line
    char** lines = (char**)malloc(sizeof(char*));
    char* token = strtok(buffer, "\n");
    int count = 0;
    while (token != NULL) {
        lines = (char**)realloc(lines, (count + 1) * sizeof(char*));
        lines[count] = strdup(token);
        count++;
        token = strtok(NULL, "\n");
    }
    lines = (char**)realloc(lines, (count + 1) * sizeof(char*));
    lines[count] = NULL;

    free(buffer);
    return lines;
}

int main() {
    char** input = readFile("input.txt");
    printf("%d\n", solve(input));

    return 0;
}
