
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    long long lanternFishCounts[9] = {0};
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    while ((read = getline(&line, &len, file)) != -1) {
        char *token = strtok(line, ",");
        while (token != NULL) {
            int age = atoi(token);
            lanternFishCounts[age]++;
            token = strtok(NULL, ",");
        }
    }
    free(line);
    fclose(file);

    for (int i = 0; i < 256; i++) {
        long long newLanternFish = lanternFishCounts[0];
        for (int j = 0; j < 8; j++) {
            lanternFishCounts[j] = lanternFishCounts[j + 1];
        }
        lanternFishCounts[6] += newLanternFish;
        lanternFishCounts[8] = newLanternFish;
    }

    long long sum = 0;
    for (int i = 0; i < 9; i++) {
        sum += lanternFishCounts[i];
    }

    printf("%lld\n", sum);

    return 0;
}
