
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

int isMovingAway(int xPos, int yPos, int xVel, int yVel, int xMin, int xMax, int yMin, int yMax) {
    if ((xPos < xMin && xVel < 0) || (xPos > xMax && xVel > 0) || (yPos < yMin && yVel < 0)) {
        return 1;
    }
    return 0;
}

int main(void) {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    char line[100];
    fgets(line, sizeof(line), file);
    fclose(file);

    int xMin, xMax, yMin, yMax;
    sscanf(line, "target area: x=%d..%d, y=%d..%d", &xMin, &xMax, &yMin, &yMax);

    int maxY = INT_MIN;
    for (int xVel = -1000; xVel <= 1000; xVel++) {
        for (int yVel = -1000; yVel <= 1000; yVel++) {
            int xPos = 0, yPos = 0;
            int curXVel = xVel, curYVel = yVel;
            int highestY = yPos;
            while (1) {
                xPos += curXVel;
                yPos += curYVel;

                if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
                    if (highestY > maxY) {
                        maxY = highestY;
                    }
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
                if (yPos > highestY) {
                    highestY = yPos;
                }
            }
        }
    }

    printf("%d\n", maxY);
    return 0;
}
