
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int totalPositions;
    int startPosition;
} Disc;

int checkDiscs(Disc discs[], int time, int numDiscs) {
    for (int i = 0; i < numDiscs; i++) {
        int position = (discs[i].startPosition + time + i + 1) % discs[i].totalPositions;
        if (position != 0) {
            return 0;
        }
    }
    return 1;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Disc discs[100];
    int numDiscs = 0;
    char line[100];
    while (fgets(line, sizeof(line), file) != NULL) {
        int totalPositions, startPosition;
        if (sscanf(line, "Disc #%*d has %d positions; at time=0, it is at position %d.", &totalPositions, &startPosition) == 2) {
            discs[numDiscs].totalPositions = totalPositions;
            discs[numDiscs].startPosition = startPosition;
            numDiscs++;
        }
    }

    // Add the new disc as per Part Two's requirement
    discs[numDiscs].totalPositions = 11;
    discs[numDiscs].startPosition = 0;
    numDiscs++;

    int time = 0;
    while (1) {
        if (checkDiscs(discs, time, numDiscs)) {
            printf("%d\n", time);
            break;
        }
        time++;
    }

    fclose(file);
    return 0;
}
