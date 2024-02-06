
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int fishes[9] = {0};
    char line[1024];
    if (fgets(line, sizeof(line), file) != NULL) {
        char *token = strtok(line, ",");
        while (token != NULL) {
            int fish = atoi(token);
            fishes[fish]++;
            token = strtok(NULL, ",");
        }
    }
    fclose(file);

    for (int day = 1; day <= 80; day++) {
        int newFish = fishes[0];
        for (int i = 1; i < 9; i++) {
            fishes[i - 1] = fishes[i];
        }
        fishes[6] += newFish;
        fishes[8] = newFish;
    }

    int totalFish = 0;
    for (int i = 0; i < 9; i++) {
        totalFish += fishes[i];
    }

    printf("%d\n", totalFish);
    return 0;
}
