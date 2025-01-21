
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define SIDE 5
#define SQUARE (SIDE * SIDE)
#define MAX_LEVELS 405

typedef struct {
    bool grid[SQUARE];
    bool exists;
} Level;

Level levels[MAX_LEVELS];
int minLevel = 200;
int maxLevel = 200;

void parse() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    char line[10];
    for (int row = 0; row < SIDE; row++) {
        if (fgets(line, sizeof(line), file) == NULL) {
             perror("Error reading file");
            exit(EXIT_FAILURE);
        }
        for (int col = 0; col < SIDE; col++) {
           levels[200].grid[row * SIDE + col] = line[col] == '#';
        }
    }
    levels[200].exists = true;
    fclose(file);
}


bool infested(int level, int cell) {
    if(level < 0 || level >= MAX_LEVELS) return false;
    if(!levels[level].exists) return false;
    return levels[level].grid[cell];
}


void next2() {
    Level newLevels[MAX_LEVELS];
    memset(newLevels, 0, sizeof(newLevels));

    int newMinLevel = minLevel;
    int newMaxLevel = maxLevel;

    for (int level = minLevel - 1; level <= maxLevel + 1; level++) {
        newLevels[level].exists = true;
        for (int cell = 0; cell < SQUARE; cell++) {
             if (cell == 12) continue;

            int row = cell / SIDE;
            int col = cell % SIDE;
            int neighbours = 0;


           if (row == 0) {
                if (infested(level - 1, 7)) neighbours++;
            }
            if (col == 0) {
                if (infested(level - 1, 11)) neighbours++;
            }
            if (col == SIDE-1) {
                if (infested(level - 1, 13)) neighbours++;
            }
            if (row == SIDE-1) {
                if (infested(level - 1, 17)) neighbours++;
            }


            if (cell == 7) {
                for (int i = 0; i < SIDE; i++) {
                   if (infested(level + 1, i)) neighbours++;
                }
            }
            if (cell == 11) {
                 for (int i = 0; i < SIDE; i++) {
                   if (infested(level + 1, 5*i)) neighbours++;
                }
            }
            if (cell == 13) {
                 for (int i = 0; i < SIDE; i++) {
                   if (infested(level + 1, 5 * i + SIDE - 1)) neighbours++;
                }
            }

            if (cell == 17) {
                 for (int i = 0; i < SIDE; i++) {
                   if (infested(level + 1, (SIDE - 1) * SIDE + i)) neighbours++;
                }
            }
          
            if (row > 0 && cell != 17) {
              if(infested(level, cell - SIDE)) neighbours++;
            }
            if (col > 0 && cell != 13) {
                 if(infested(level, cell - 1)) neighbours++;
            }

            if (col < SIDE - 1 && cell != 11) {
               if(infested(level, cell + 1)) neighbours++;
            }
           if (row < SIDE - 1 && cell != 7) {
                if(infested(level, cell + SIDE)) neighbours++;
            }


            if (infested(level, cell) && neighbours != 1) {
                newLevels[level].grid[cell] = false;
                continue;
            }

            if (!infested(level, cell) && (neighbours == 1 || neighbours == 2)) {
                newLevels[level].grid[cell] = true;
                continue;
            }

            newLevels[level].grid[cell] = infested(level, cell);
        }
         if (level < newMinLevel && newLevels[level].exists) newMinLevel = level;
         if (level > newMaxLevel && newLevels[level].exists) newMaxLevel = level;
    }

    memcpy(levels, newLevels, sizeof(levels));
    minLevel = newMinLevel;
    maxLevel = newMaxLevel;

    
    int countMin = 0, countMax = 0;
      for(int cell = 0; cell < SQUARE; cell++) {
        if(levels[minLevel].grid[cell]) countMin++;
        if(levels[maxLevel].grid[cell]) countMax++;
      }

      if(countMin == 0 && minLevel != 200) levels[minLevel].exists = false;
      if(countMax == 0 && maxLevel != 200) levels[maxLevel].exists = false;

     while(!levels[minLevel].exists) minLevel++;
     while(!levels[maxLevel].exists) maxLevel--;

}

int main() {
    parse();

    for (int i = 0; i < 200; i++) {
        next2();
    }

    int count = 0;
    for(int level = minLevel; level <= maxLevel; level++){
        if(levels[level].exists){
            for (int i = 0; i < SQUARE; i++) {
                if (levels[level].grid[i]) {
                    count++;
                }
            }
        }
    }
    printf("%d\n", count);

    return 0;
}
