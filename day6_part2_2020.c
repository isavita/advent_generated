
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int totalCount = 0;
    char line[1000];
    int groupAnswers[256] = {0};
    int groupSize = 0;

    while (fgets(line, sizeof(line), file)) {
        if (line[0] == '\n') {
            for (int i = 0; i < 256; i++) {
                if (groupAnswers[i] == groupSize) {
                    totalCount++;
                }
                groupAnswers[i] = 0;
            }
            groupSize = 0;
        } else {
            groupSize++;
            for (int i = 0; i < strlen(line); i++) {
                if (line[i] != '\n') {
                    groupAnswers[(int)line[i]]++;
                }
            }
        }
    }

    for (int i = 0; i < 256; i++) {
        if (groupAnswers[i] == groupSize) {
            totalCount++;
        }
    }

    printf("%d\n", totalCount);

    fclose(file);
    return 0;
}
