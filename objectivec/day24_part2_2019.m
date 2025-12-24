
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define SIDE 5
#define SQUARE (SIDE * SIDE)
#define MAX_LEVELS 405
#define START_LEVEL 200

typedef struct {
    bool grid[SQUARE];
    bool exists;
} Level;

static Level levels[MAX_LEVELS];
static int minLevel = START_LEVEL;
static int maxLevel = START_LEVEL;

static void parse(void) {
    FILE *f = fopen("input.txt", "r");
    if (!f) exit(EXIT_FAILURE);
    char line[10];
    for (int r = 0; r < SIDE; r++) {
        if (!fgets(line, sizeof(line), f)) exit(EXIT_FAILURE);
        for (int c = 0; c < SIDE; c++) {
            levels[START_LEVEL].grid[r * SIDE + c] = line[c] == '#';
        }
    }
    levels[START_LEVEL].exists = true;
    fclose(f);
}

static inline bool infested(int lvl, int cell) {
    return (lvl >= 0 && lvl < MAX_LEVELS && levels[lvl].exists && levels[lvl].grid[cell]);
}

static void next2(void) {
    Level newLevels[MAX_LEVELS];
    memset(newLevels, 0, sizeof(newLevels));
    int newMin = minLevel, newMax = maxLevel;
    for (int lvl = minLevel - 1; lvl <= maxLevel + 1; lvl++) {
        newLevels[lvl].exists = true;
        for (int cell = 0; cell < SQUARE; cell++) {
            if (cell == 12) continue;
            int row = cell / SIDE, col = cell % SIDE, n = 0;
            if (row == 0 && infested(lvl - 1, 7)) n++;
            if (col == 0 && infested(lvl - 1, 11)) n++;
            if (col == SIDE - 1 && infested(lvl - 1, 13)) n++;
            if (row == SIDE - 1 && infested(lvl - 1, 17)) n++;
            if (cell == 7)  for (int i = 0; i < SIDE; i++) if (infested(lvl + 1, i)) n++;
            if (cell == 11) for (int i = 0; i < SIDE; i++) if (infested(lvl + 1, 5 * i)) n++;
            if (cell == 13) for (int i = 0; i < SIDE; i++) if (infested(lvl + 1, 5 * i + SIDE - 1)) n++;
            if (cell == 17) for (int i = 0; i < SIDE; i++) if (infested(lvl + 1, (SIDE - 1) * SIDE + i)) n++;
            if (row > 0 && cell != 17 && infested(lvl, cell - SIDE)) n++;
            if (col > 0 && cell != 13 && infested(lvl, cell - 1)) n++;
            if (col < SIDE - 1 && cell != 11 && infested(lvl, cell + 1)) n++;
            if (row < SIDE - 1 && cell != 7 && infested(lvl, cell + SIDE)) n++;
            bool cur = infested(lvl, cell);
            newLevels[lvl].grid[cell] = (cur && n == 1) || (!cur && (n == 1 || n == 2));
        }
        if (newLevels[lvl].exists) {
            if (lvl < newMin) newMin = lvl;
            if (lvl > newMax) newMax = lvl;
        }
    }
    memcpy(levels, newLevels, sizeof(levels));
    minLevel = newMin;
    maxLevel = newMax;
    while (minLevel < START_LEVEL && !levels[minLevel].exists) minLevel++;
    while (maxLevel > START_LEVEL && !levels[maxLevel].exists) maxLevel--;
    int cntMin = 0, cntMax = 0;
    for (int i = 0; i < SQUARE; i++) {
        if (levels[minLevel].grid[i]) cntMin++;
        if (levels[maxLevel].grid[i]) cntMax++;
    }
    if (!cntMin && minLevel != START_LEVEL) levels[minLevel].exists = false;
    if (!cntMax && maxLevel != START_LEVEL) levels[maxLevel].exists = false;
    while (minLevel < START_LEVEL && !levels[minLevel].exists) minLevel++;
    while (maxLevel > START_LEVEL && !levels[maxLevel].exists) maxLevel--;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        parse();
        for (int i = 0; i < 200; i++) next2();
        int total = 0;
        for (int lvl = minLevel; lvl <= maxLevel; lvl++) if (levels[lvl].exists)
            for (int c = 0; c < SQUARE; c++) if (levels[lvl].grid[c]) total++;
        printf("%d\n", total);
    }
    return 0;
}
