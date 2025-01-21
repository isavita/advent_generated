
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

#define MAX_ELVES 2000
#define MAP_SIZE 1000

typedef struct {
    int x, y;
} Point;

typedef struct {
    Point pos;
    bool moving;
    Point nextPos;
} Elf;

const int N = 1;
const int E = 3;
const int S = 5;
const int W = 7;

Point Dirs[8] = {{-1, -1}, {-1, 0}, {-1, 1}, {0, 1}, {1, 1}, {1, 0}, {1, -1}, {0, -1}};
int Order[4] = {N, S, W, E};
int currDir = 0;

Elf* elves[MAX_ELVES];
int numElves = 0;
bool map[MAP_SIZE][MAP_SIZE];

int hash(int x, int y) {
    return (x + MAP_SIZE/2) * MAP_SIZE + (y + MAP_SIZE/2);
}

void unhash(int hash, int* x, int* y) {
    *x = hash / MAP_SIZE - MAP_SIZE/2;
    *y = hash % MAP_SIZE - MAP_SIZE/2;
}

bool isOccupied(int x, int y) {
    if (x < -MAP_SIZE/2 || x >= MAP_SIZE/2 || y < -MAP_SIZE/2 || y >= MAP_SIZE/2) return false;
    return map[x + MAP_SIZE/2][y + MAP_SIZE/2];
}

bool aroundAllEmpty(Elf* elf) {
    for (int i = 0; i < 8; i++) {
        if (isOccupied(elf->pos.x + Dirs[i].x, elf->pos.y + Dirs[i].y)) return false;
    }
    return true;
}

bool elfInDirection(Elf* elf, int dir) {
    for (int j = -1; j <= 1; j++) {
        int d = (dir + j + 8) % 8;
        if (isOccupied(elf->pos.x + Dirs[d].x, elf->pos.y + Dirs[d].y)) return true;
    }
    return false;
}

bool run() {
    int proposes[MAP_SIZE * MAP_SIZE] = {0};
    bool someoneMoved = false;

    for(int i = 0; i < numElves; i++) {
        Elf* elf = elves[i];
        elf->moving = false;

        if (aroundAllEmpty(elf)) continue;

        for (int j = 0; j < 4; j++) {
            int dir = Order[(currDir + j) % 4];
            if (elfInDirection(elf, dir)) continue;

            elf->nextPos.x = elf->pos.x + Dirs[dir].x;
            elf->nextPos.y = elf->pos.y + Dirs[dir].y;
            proposes[hash(elf->nextPos.x, elf->nextPos.y)]++;
            elf->moving = true;
            break;
        }
    }
    
    for(int i = 0; i < numElves; i++) {
        Elf* elf = elves[i];
        if(!elf->moving) continue;
        
        if(proposes[hash(elf->nextPos.x, elf->nextPos.y)] > 1) continue;
        
        someoneMoved = true;
        map[elf->pos.x + MAP_SIZE/2][elf->pos.y + MAP_SIZE/2] = false;
        map[elf->nextPos.x + MAP_SIZE/2][elf->nextPos.y + MAP_SIZE/2] = true;
        elf->pos = elf->nextPos;
        elf->moving = false;
    }

    currDir = (currDir + 1) % 4;
    return someoneMoved;
}

int main() {
    FILE* fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[200];
    int row = 0;
    while (fgets(line, sizeof(line), fp)) {
        for (int col = 0; line[col] != '\0'; col++) {
            if (line[col] == '#') {
                Elf* elf = (Elf*)malloc(sizeof(Elf));
                if (elf == NULL) {
                    perror("Memory allocation failed");
                    fclose(fp);
                    return 1;
                }
                elf->pos.x = row;
                elf->pos.y = col;
                elf->moving = false;
                elves[numElves++] = elf;
                map[row + MAP_SIZE/2][col + MAP_SIZE/2] = true;
            }
        }
        row++;
    }
    fclose(fp);

    for (int i = 0; ; i++) {
        if (!run()) {
            printf("%d\n", i + 1);
            for(int j = 0; j < numElves; j++){
                free(elves[j]);
            }
            return 0;
        }
    }
}
