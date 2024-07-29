#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 256
#define MAX_PROGRAMS 128

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) return 1;

    char line[MAX_LINE_LENGTH];
    char *holderMap[MAX_PROGRAMS] = {0}, *heldMap[MAX_PROGRAMS] = {0};
    int holderCount = 0, heldCount = 0;

    while (fgets(line, sizeof(line), file)) {
        char *token = strtok(line, " ");
        if (!token || holderCount >= MAX_PROGRAMS) continue;
        holderMap[holderCount++] = strdup(token);

        while ((token = strtok(NULL, " ,\n"))) {
            if (heldCount < MAX_PROGRAMS) {
                heldMap[heldCount++] = strdup(token);
            }
        }
    }
    fclose(file);

    for (int i = 0; i < holderCount; i++) {
        int isHeld = 0;
        for (int j = 0; j < heldCount; j++) {
            if (strcmp(holderMap[i], heldMap[j]) == 0) {
                isHeld = 1;
                break;
            }
        }
        if (!isHeld) {
            printf("%s\n", holderMap[i]);
        }
    }

    for (int i = 0; i < holderCount; i++) free(holderMap[i]);
    for (int i = 0; i < heldCount; i++) free(heldMap[i]);

    return 0;
}