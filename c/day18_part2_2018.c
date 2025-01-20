
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define SIZE 50

typedef struct {
    char grid[SIZE][SIZE];
} Grid;

typedef struct {
    char grid[SIZE][SIZE];
    int hash;
} GridState;


int calculateHash(const Grid *grid) {
    int hash = 0;
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            hash = hash * 31 + grid->grid[i][j];
        }
    }
    return hash;
}


Grid *readInput(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        return NULL;
    }

    Grid *grid = malloc(sizeof(Grid));
    if (!grid) {
        fclose(file);
        perror("Memory allocation failed");
        return NULL;
    }

    for (int i = 0; i < SIZE; i++) {
        if (fgets(grid->grid[i], SIZE + 2, file) == NULL) {
            free(grid);
            fclose(file);
            perror("Error reading file");
            return NULL;
        }
        grid->grid[i][strcspn(grid->grid[i], "\r\n")] = '\0';
    }

    fclose(file);
    return grid;
}


void transform(Grid *grid, Grid *newGrid) {
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            int adjacentTrees = 0;
            int adjacentLumberyards = 0;

            for (int x = -1; x <= 1; x++) {
                for (int y = -1; y <= 1; y++) {
                    if (x == 0 && y == 0) continue;
                    int ni = i + x;
                    int nj = j + y;
                    if (ni >= 0 && ni < SIZE && nj >= 0 && nj < SIZE) {
                         if (grid->grid[ni][nj] == '|') adjacentTrees++;
                         else if (grid->grid[ni][nj] == '#') adjacentLumberyards++;
                    }
                }
            }

             if (grid->grid[i][j] == '.') {
                 newGrid->grid[i][j] = (adjacentTrees >= 3) ? '|' : '.';
              } else if (grid->grid[i][j] == '|') {
                  newGrid->grid[i][j] = (adjacentLumberyards >= 3) ? '#' : '|';
              } else if (grid->grid[i][j] == '#') {
                   newGrid->grid[i][j] = (adjacentLumberyards >= 1 && adjacentTrees >= 1) ? '#' : '.';
            }
        }
    }
}


void countResources(const Grid *grid, int *wooded, int *lumberyards) {
    *wooded = 0;
    *lumberyards = 0;
    for (int i = 0; i < SIZE; i++) {
        for (int j = 0; j < SIZE; j++) {
            if (grid->grid[i][j] == '|') (*wooded)++;
            else if (grid->grid[i][j] == '#') (*lumberyards)++;
        }
    }
}


int main() {
    Grid *grid = readInput("input.txt");
    if (!grid) return 1;
    Grid *newGrid = malloc(sizeof(Grid));
    if (!newGrid) {
        free(grid);
        return 1;
    }
    
    GridState *seenStates = malloc(sizeof(GridState) * 1000);
    int seenCount = 0;

    int cycleStart = 0, cycleLength = 0;
    int minute = 0;

    for (; ; minute++) {
        int hash = calculateHash(grid);
        bool found = false;
          for (int i = 0; i < seenCount; i++) {
               if (seenStates[i].hash == hash) {
                   bool isEqual = true;
                   for (int r = 0; r < SIZE; r++) {
                       for (int c = 0; c < SIZE; c++){
                           if (seenStates[i].grid[r][c] != grid->grid[r][c]){
                               isEqual = false;
                               break;
                           }
                       }
                       if (!isEqual) break;
                   }
                   if (isEqual){
                       cycleStart = i;
                        cycleLength = minute - i;
                       found = true;
                       break;
                   }
               }
            }
        
        if (found) break;
          
       if (seenCount < 1000) {
          seenStates[seenCount].hash = hash;
           memcpy(seenStates[seenCount].grid, grid->grid, sizeof(grid->grid));
             seenCount++;
          }
        transform(grid,newGrid);
        memcpy(grid->grid, newGrid->grid,sizeof(grid->grid));

    }

    int remainingMinutes = (1000000000 - cycleStart) % cycleLength;
    for (int i = 0; i < remainingMinutes; i++) {
      transform(grid,newGrid);
       memcpy(grid->grid, newGrid->grid,sizeof(grid->grid));
    }

    int wooded, lumberyards;
    countResources(grid, &wooded, &lumberyards);
    printf("%d\n", wooded * lumberyards);


    free(grid);
    free(newGrid);
    free(seenStates);
    return 0;
}
