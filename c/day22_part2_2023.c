
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_BRICKS 2000

typedef struct {
    int x, y, z;
} Coord;

typedef struct Brick {
    Coord mini;
    Coord maxi;
    struct Brick *basedOn[MAX_BRICKS];
    int basedOnCount;
    struct Brick *support[MAX_BRICKS];
    int supportCount;
} Brick;

int max(int a, int b) {
    return a > b ? a : b;
}

int min(int a, int b) {
    return a < b ? a : b;
}

int compareBricks(const void *a, const void *b) {
    const Brick *brickA = *(const Brick **)a;
    const Brick *brickB = *(const Brick **)b;
    return brickA->maxi.z - brickB->maxi.z;
}

Brick* parseInput(char *input, int *brickCount) {
    Brick *bricks = (Brick*) malloc(sizeof(Brick) * MAX_BRICKS);
    char *line = strtok(input, "\n");
    int i = 0;
    while (line != NULL) {
        sscanf(line, "%d,%d,%d~%d,%d,%d", &bricks[i].mini.x, &bricks[i].mini.y, &bricks[i].mini.z, &bricks[i].maxi.x, &bricks[i].maxi.y, &bricks[i].maxi.z);
        bricks[i].basedOnCount = 0;
        bricks[i].supportCount = 0;
        line = strtok(NULL, "\n");
        i++;
    }
    *brickCount = i;
    return bricks;
}

void settle(Brick *bricks, int brickCount) {
    Brick *brickPointers[MAX_BRICKS];
    for (int i = 0; i < brickCount; i++) {
        brickPointers[i] = &bricks[i];
    }
    qsort(brickPointers, brickCount, sizeof(Brick*), compareBricks);

    for (int i = 0; i < brickCount; i++) {
        Brick *brick = brickPointers[i];
        int supportZ = 0;
        int basedCount = 0;
         for (int j = i - 1; j >= 0; j--) {
            Brick *prevBrick = brickPointers[j];
            bool isIntersectingX = max(brick->mini.x, prevBrick->mini.x) <= min(brick->maxi.x, prevBrick->maxi.x);
            bool isIntersectingY = max(brick->mini.y, prevBrick->mini.y) <= min(brick->maxi.y, prevBrick->maxi.y);
            if (isIntersectingX && isIntersectingY) {
                if (prevBrick->maxi.z == supportZ) {
                    brick->basedOn[basedCount++] = prevBrick;
                } else if (prevBrick->maxi.z > supportZ) {
                    supportZ = prevBrick->maxi.z;
                    basedCount = 0;
                    brick->basedOn[basedCount++] = prevBrick;
                 }
            }
        }
        brick->basedOnCount = basedCount;
        for (int j = 0; j < basedCount; j++) {
              brick->basedOn[j]->support[brick->basedOn[j]->supportCount++] = brick;
        }
        int deltaZ = brick->maxi.z - brick->mini.z;
        brick->mini.z = supportZ + 1;
        brick->maxi.z = brick->mini.z + deltaZ;
    }
}

int solve(Brick *bricks, int brickCount) {
    settle(bricks, brickCount);
    int count = 0;
    for (int i = 0; i < brickCount; i++) {
        Brick *brick = &bricks[i];
        int fallingBricks[MAX_BRICKS] = {0};
        int fallingCount = 0;
        for (int j = 0; j < brick->supportCount; j++) {
            Brick *supportedBrick = brick->support[j];
            if (supportedBrick->basedOnCount == 1) {
                Brick *allSupportedBricks[MAX_BRICKS] = {0};
                int allSupportedCount = 0;
                allSupportedBricks[allSupportedCount++] = supportedBrick;

                int processed[MAX_BRICKS] = {0};
                while (allSupportedCount > 0) {
                  Brick *supportedBrick0 = allSupportedBricks[--allSupportedCount];
                    bool isFalling = true;
                    for (int k = 0; k < supportedBrick0->basedOnCount; k++) {
                         Brick *basedBrick = supportedBrick0->basedOn[k];
                         bool found = false;
                        for(int m=0;m<fallingCount; m++){
                            if(&bricks[fallingBricks[m]] == basedBrick){
                                found=true;
                                break;
                            }
                         }
                         if (basedBrick != brick && !found) {
                             isFalling = false;
                            break;
                         }
                     }

                     if (isFalling) {
                         bool found = false;
                         for(int m=0;m<fallingCount; m++){
                            if(&bricks[fallingBricks[m]] == supportedBrick0){
                                found=true;
                                break;
                            }
                         }
                        if(!found){
                            fallingBricks[fallingCount++] = supportedBrick0 - bricks;
                           for(int k=0; k < supportedBrick0->supportCount; k++){
                              allSupportedBricks[allSupportedCount++] = supportedBrick0->support[k];
                            }
                         }
                     }
                }
           }
        }
         count += fallingCount;
    }

    return count;
}

int main() {
     FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *buffer = (char *)malloc(file_size + 1);
    if (buffer == NULL) {
        perror("Memory allocation error");
        fclose(fp);
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, fp);
    if (bytes_read != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    buffer[file_size] = '\0';
    fclose(fp);
    
    int brickCount;
    Brick *bricks = parseInput(buffer, &brickCount);
    free(buffer);
    
    int result = solve(bricks, brickCount);
    free(bricks);
    printf("%d\n", result);

    return 0;
}
