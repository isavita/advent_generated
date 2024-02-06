
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    char *lines[500]; // Assuming maximum 500 lines
    int lineCount = 0;
    char buffer[255]; // Assuming each line has a maximum of 254 characters
    while (fgets(buffer, 255, file) != NULL) {
        buffer[strcspn(buffer, "\n")] = 0; // Remove newline character
        lines[lineCount] = (char *)malloc(strlen(buffer) + 1);
        strcpy(lines[lineCount], buffer);
        lineCount++;
    }
    fclose(file);

    int slopes[5][2] = {
        {1, 1},
        {3, 1},
        {5, 1},
        {7, 1},
        {1, 2},
    };

    long long product = 1;
    for (int j = 0; j < 5; j++) {
        int treeCount = 0, pos = 0;
        for (int i = 0; i < lineCount; i += slopes[j][1]) {
            if (lines[i][pos] == '#') {
                treeCount++;
            }
            pos = (pos + slopes[j][0]) % strlen(lines[i]);
        }
        product *= treeCount;
    }

    printf("%lld\n", product);

    // Free allocated memory
    for (int i = 0; i < lineCount; i++) {
        free(lines[i]);
    }

    return 0;
}
