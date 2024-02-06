
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("File reading error");
        return 1;
    }

    char data[100];
    fgets(data, 100, file);

    int target = atoi(data);

    int sideLength = ceil(sqrt(target));
    if (sideLength % 2 == 0) {
        sideLength++;
    }

    int maxValue = sideLength * sideLength;
    int stepsFromEdge = (sideLength - 1) / 2;
    int distanceToMiddle = 0;

    for (int i = 0; i < 4; i++) {
        int middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i;
        int distance = abs(target - middlePoint);
        if (distance < distanceToMiddle || i == 0) {
            distanceToMiddle = distance;
        }
    }

    int manhattanDistance = stepsFromEdge + distanceToMiddle;

    printf("%d\n", manhattanDistance);

    fclose(file);
    return 0;
}
