
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int speed;
    int flyTime;
    int restTime;
    int distance;
    int points;
    int flying;
    int timeInMode;
} Reindeer;

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    char line[256];
    int reindeerCount = 0;
    while (fgets(line, sizeof(line), file) != NULL) {
        reindeerCount++;
    }
    fseek(file, 0, SEEK_SET);

    Reindeer *reindeers = (Reindeer *)malloc(reindeerCount * sizeof(Reindeer));
    int i = 0;
    while (fgets(line, sizeof(line), file) != NULL) {
        sscanf(line, "%*s %*s %*s %d %*s %*s %d %*s %*s %*s %*s %*s %*s %d",
               &reindeers[i].speed, &reindeers[i].flyTime, &reindeers[i].restTime);
        reindeers[i].distance = 0;
        reindeers[i].points = 0;
        reindeers[i].flying = 1;
        reindeers[i].timeInMode = 0;
        i++;
    }
    fclose(file);

    int totalSeconds = 2503;
    for (int i = 0; i < totalSeconds; i++) {
        int maxDistance = 0;
        for (int j = 0; j < reindeerCount; j++) {
            if (reindeers[j].flying) {
                reindeers[j].distance += reindeers[j].speed;
            }
            reindeers[j].timeInMode++;
            if ((reindeers[j].flying && reindeers[j].timeInMode == reindeers[j].flyTime) ||
                (!reindeers[j].flying && reindeers[j].timeInMode == reindeers[j].restTime)) {
                reindeers[j].flying = !reindeers[j].flying;
                reindeers[j].timeInMode = 0;
            }
            if (reindeers[j].distance > maxDistance) {
                maxDistance = reindeers[j].distance;
            }
        }
        for (int j = 0; j < reindeerCount; j++) {
            if (reindeers[j].distance == maxDistance) {
                reindeers[j].points++;
            }
        }
    }

    int maxPoints = 0;
    for (int i = 0; i < reindeerCount; i++) {
        if (reindeers[i].points > maxPoints) {
            maxPoints = reindeers[i].points;
        }
    }

    printf("%d\n", maxPoints);
    free(reindeers);

    return 0;
}
