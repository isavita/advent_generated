
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int *changes = NULL;
    int numChanges = 0;

    while ((read = getline(&line, &len, file)) != -1) {
        numChanges++;
        changes = realloc(changes, numChanges * sizeof(int));
        changes[numChanges - 1] = atoi(line);
    }
    fclose(file);
    free(line);

    int *seenFrequencies = calloc(200000, sizeof(int));
    int currentFrequency = 0;
    seenFrequencies[currentFrequency + 100000] = 1;

    int found = 0;
    while (!found) {
        for (int i = 0; i < numChanges; i++) {
            currentFrequency += changes[i];
            if (currentFrequency >= -100000 && currentFrequency <= 100000 && seenFrequencies[currentFrequency + 100000]) {
                printf("%d\n", currentFrequency);
                found = 1;
                break;
            }
            if (currentFrequency >= -100000 && currentFrequency <= 100000) {
              seenFrequencies[currentFrequency + 100000] = 1;
            }
        }
    }

    free(changes);
    free(seenFrequencies);

    return 0;
}
