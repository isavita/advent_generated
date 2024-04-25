#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int calculateMemoryLength(char *s) {
    int length = 0;
    int inEscape = 0;
    int hexCount = 0;

    for (int i = 1; i < strlen(s) - 1; i++) {
        if (hexCount > 0) {
            hexCount--;
        } else if (inEscape) {
            if (s[i] == 'x') {
                hexCount = 2;
            }
            inEscape = 0;
            length++;
        } else if (s[i] == '\\') {
            inEscape = 1;
        } else {
            length++;
        }
    }
    return length;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file.\n");
        return 1;
    }

    int totalDiff = 0;
    char line[1024];

    while (fgets(line, 1024, file)) {
        int codeLength = strlen(line);
        int memoryLength = calculateMemoryLength(line);
        totalDiff += codeLength - memoryLength;
    }

    fclose(file);
    printf("%d\n", totalDiff);
    return 0;
}