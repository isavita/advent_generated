
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <float.h>

typedef struct {
    int x, y;
    double angle;
    double dist;
} asteroid;

int readAsteroids(const char *filename, bool ***asteroids_ptr, int *rows, int *cols) {
    FILE *file = fopen(filename, "r");
    if (!file) return 1;

    char line[1024];
    *rows = 0;
    *cols = 0;
    *asteroids_ptr = NULL;
    
    while (fgets(line, sizeof(line), file)) {
        int len = 0;
        while(line[len] && line[len] != '\n') len++;
        if (*rows == 0) {
            *cols = len;
        }
         
        bool *asteroidRow = malloc(len * sizeof(bool));
        if (!asteroidRow) {
            fclose(file);
            return 1;
        }

        for (int i = 0; i < len; i++) {
            asteroidRow[i] = line[i] == '#';
        }
        
        *asteroids_ptr = realloc(*asteroids_ptr, (*rows + 1) * sizeof(bool*));
        if (!*asteroids_ptr) {
            free(asteroidRow);
            fclose(file);
             return 1;
        }
        (*asteroids_ptr)[*rows] = asteroidRow;
        (*rows)++;
    }

    fclose(file);
    return 0;
}

int compareAsteroids(const void *a, const void *b) {
    const asteroid *astA = (const asteroid *)a;
    const asteroid *astB = (const asteroid *)b;
    if (astA->angle < astB->angle) return -1;
    if (astA->angle > astB->angle) return 1;
    if (astA->dist < astB->dist) return -1;
    if (astA->dist > astB->dist) return 1;
    return 0;
}


asteroid* vaporizeAsteroids(bool **asteroids, int rows, int cols, int stationX, int stationY, int *vaporizedCount) {
    asteroid *targets = NULL;
    int targetCount = 0;
    
    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) {
             if (asteroids[y][x] && !(x == stationX && y == stationY)) {
                double angle = atan2((double)(y - stationY), (double)(x - stationX));
                double dist = hypot((double)(x - stationX), (double)(y - stationY));
                 if (angle < -M_PI / 2) {
                    angle += 2 * M_PI;
                }
                targets = realloc(targets, (targetCount + 1) * sizeof(asteroid));
                 if (!targets) {
                   *vaporizedCount = 0;
                    return NULL;
                }
                targets[targetCount].x = x;
                targets[targetCount].y = y;
                targets[targetCount].angle = angle;
                targets[targetCount].dist = dist;
                targetCount++;
            }
        }
    }

    qsort(targets, targetCount, sizeof(asteroid), compareAsteroids);

    asteroid* vaporized = malloc(targetCount * sizeof(asteroid));
    if(!vaporized) {
        *vaporizedCount = 0;
        free(targets);
        return NULL;
    }
    
    int vaporizedIndex = 0;
    
    while (targetCount > 0) {
        double lastAngle = -DBL_MAX;
        int i = 0;
        int newTargetCount = 0;
        while(i < targetCount)
        {
            if (targets[i].angle != lastAngle) {
               vaporized[vaporizedIndex] = targets[i];
               lastAngle = targets[i].angle;
               vaporizedIndex++;
            } else {
               targets[newTargetCount] = targets[i];
               newTargetCount++;
            }
            i++;
        }
        targetCount = newTargetCount;
    }

    *vaporizedCount = vaporizedIndex;
    free(targets);
    return vaporized;
}


void findBestAsteroidLocation(bool **asteroids, int rows, int cols, int *bestX, int *bestY, int *maxCount) {
    *maxCount = 0;
    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) {
            if (asteroids[y][x]) {
                 int count = 0;
                double angles[rows * cols];
                int angleCount = 0;
                 for (int otherY = 0; otherY < rows; otherY++) {
                    for (int otherX = 0; otherX < cols; otherX++) {
                        if (asteroids[otherY][otherX] && !(otherX == x && otherY == y)) {
                            double angle = atan2((double)(otherY - y), (double)(otherX - x));
                            bool found = false;
                           for(int k = 0; k < angleCount; k++) {
                               if(fabs(angles[k] - angle) < 1e-9)
                               {
                                   found = true;
                                   break;
                               }
                           }
                            if(!found) {
                                angles[angleCount++] = angle;
                                count++;
                            }
                        }
                    }
                 }
                if (count > *maxCount) {
                    *maxCount = count;
                    *bestX = x;
                    *bestY = y;
                }
            }
        }
    }
}


int main() {
    bool **asteroids = NULL;
    int rows, cols;

    if (readAsteroids("input.txt", &asteroids, &rows, &cols) != 0) {
        fprintf(stderr, "Failed to read input.\n");
        return 1;
    }

    int stationX, stationY, maxCount;
    findBestAsteroidLocation(asteroids, rows, cols, &stationX, &stationY, &maxCount);

    int vaporizedCount;
    asteroid *vaporized = vaporizeAsteroids(asteroids, rows, cols, stationX, stationY, &vaporizedCount);
    
    if(vaporized == NULL) {
         fprintf(stderr, "Failed to allocate memory.\n");
         for (int i = 0; i < rows; i++) {
           free(asteroids[i]);
         }
         free(asteroids);
         return 1;
    }

    if (vaporizedCount >= 200) {
        int result = vaporized[199].x * 100 + vaporized[199].y;
        printf("%d\n", result);
    } else {
        printf("Less than 200 asteroids were vaporized.\n");
    }
    
    free(vaporized);

     for (int i = 0; i < rows; i++) {
        free(asteroids[i]);
    }
    free(asteroids);
    return 0;
}
