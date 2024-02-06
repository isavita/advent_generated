
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NUMBERS 30000000

int main() {
    FILE *file;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error reading file");
        return 1;
    }

    if ((read = getline(&line, &len, file)) != -1) {
        line[strcspn(line, "\n")] = 0; // Remove newline character
    } else {
        printf("Error reading the line from the file\n");
        fclose(file);
        if (line) free(line);
        return 1;
    }
    fclose(file);

    int *spoken = calloc(MAX_NUMBERS, sizeof(int));
    if (spoken == NULL) {
        perror("Failed to allocate memory");
        free(line);
        return 1;
    }

    char *token = strtok(line, ",");
    int lastSpoken = 0;
    int turn = 1;
    while (token != NULL) {
        lastSpoken = atoi(token);
        if (turn < MAX_NUMBERS) {
            spoken[lastSpoken] = turn;
        }
        token = strtok(NULL, ",");
        turn++;
    }
    turn--; // Adjust for the extra increment

    for (; turn < MAX_NUMBERS; turn++) {
        int nextNumber = 0;
        if (spoken[lastSpoken] != 0) {
            nextNumber = turn - spoken[lastSpoken];
        }
        spoken[lastSpoken] = turn;
        lastSpoken = nextNumber;
    }

    printf("%d\n", lastSpoken);

    free(line);
    free(spoken);
    return 0;
}
