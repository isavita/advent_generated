
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int isMovingAway(int xPos, int yPos, int xVel, int yVel, int xMin, int xMax, int yMin, int yMax) {
    if ((xPos < xMin && xVel < 0) || (xPos > xMax && xVel > 0) || (yPos < yMin && yVel < 0)) {
        return 1;
    }
    return 0;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int xMin, xMax, yMin, yMax;
    fscanf(file, "target area: x=%d..%d, y=%d..%d", &xMin, &xMax, &yMin, &yMax);
    fclose(file);

    int uniqueVelocities = 0;
    for (int xVel = -1000; xVel <= 1000; xVel++) {
        for (int yVel = -1000; yVel <= 1000; yVel++) {
            int xPos = 0, yPos = 0, curXVel = xVel, curYVel = yVel, inTargetArea = 0;
            while (1) {
                xPos += curXVel;
                yPos += curYVel;

                if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
                    inTargetArea = 1;
                    break;
                }

                if (isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax)) {
                    break;
                }

                if (curXVel > 0) {
                    curXVel--;
                } else if (curXVel < 0) {
                    curXVel++;
                }

                curYVel--;
            }

            uniqueVelocities += inTargetArea;
        }
    }

    printf("%d\n", uniqueVelocities);
    return 0;
}
