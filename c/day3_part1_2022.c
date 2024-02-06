
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int itemPriority(char item) {
    if (item >= 'a' && item <= 'z') {
        return (int)(item - 'a' + 1);
    }
    return (int)(item - 'A') + 27;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file");
        return 1;
    }

    int sum = 0;
    char line[1000];
    while (fgets(line, sizeof(line), file) != NULL) {
        int half = strlen(line) / 2;
        char firstCompartment[500];
        char secondCompartment[500];

        strncpy(firstCompartment, line, half);
        firstCompartment[half] = '\0';
        strncpy(secondCompartment, line + half, half);
        secondCompartment[half] = '\0';

        int compartmentMap[128] = {0};
        for (int i = 0; i < half; i++) {
            compartmentMap[firstCompartment[i]]++;
        }
        for (int i = 0; i < half; i++) {
            if (compartmentMap[secondCompartment[i]] > 0) {
                sum += itemPriority(secondCompartment[i]);
                break;
            }
        }
    }

    fclose(file);

    printf("%d\n", sum);
    return 0;
}
