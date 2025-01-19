
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_GRID_SIZE 150
#define MAX_BEAMS 20000

typedef struct {
    int x;
    int y;
} Coord;

typedef struct {
    Coord origin;
    Coord dir;
} Beam;

typedef struct {
    int width;
    int height;
    char data[MAX_GRID_SIZE][MAX_GRID_SIZE];
} Grid;

const Coord NORTH = {0, -1};
const Coord WEST = {-1, 0};
const Coord SOUTH = {0, 1};
const Coord EAST = {1, 0};

Coord rotate90(Coord coord) {
    Coord result = {coord.y, -coord.x};
    return result;
}

Coord rotateNeg90(Coord coord) {
    Coord result = {-coord.y, coord.x};
    return result;
}

bool isInBounds(Coord coord, Grid grid) {
    return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height;
}

void buildGrid(Grid *grid, char input[MAX_GRID_SIZE][MAX_GRID_SIZE], int numRows, int numCols) {
    grid->width = numCols;
    grid->height = numRows;
    
    for (int y = 0; y < numRows; y++) {
        for (int x = 0; x < numCols; x++) {
            grid->data[y][x] = input[y][x];
        }
    }
}

void nextBeam(Grid grid, Beam beam, Beam nextBeams[4], int *numNextBeams) {
    *numNextBeams = 0;
    int x = beam.origin.x;
    int y = beam.origin.y;
    char char_at_pos = (isInBounds(beam.origin, grid) ? grid.data[y][x] : '\0');

    if (char_at_pos == '\0' || char_at_pos == '.') {
       nextBeams[(*numNextBeams)++] = (Beam) {{beam.origin.x + beam.dir.x, beam.origin.y + beam.dir.y}, beam.dir};
       return;
    }

    switch (char_at_pos) {
        case '/': {
            Coord newDir;
            if (beam.dir.x == 0) {
                newDir = rotateNeg90(beam.dir);
            } else {
                newDir = rotate90(beam.dir);
            }
           nextBeams[(*numNextBeams)++] = (Beam) {{beam.origin.x + newDir.x, beam.origin.y + newDir.y}, newDir};
            break;
        }
        case '\\': {
            Coord newDir;
            if (beam.dir.x == 0) {
                newDir = rotate90(beam.dir);
            } else {
                newDir = rotateNeg90(beam.dir);
            }
           nextBeams[(*numNextBeams)++] = (Beam) {{beam.origin.x + newDir.x, beam.origin.y + newDir.y}, newDir};
            break;
        }
        case '|': {
             if (beam.dir.x != 0) {
                  Coord newDir1 = rotate90(beam.dir);
                  nextBeams[(*numNextBeams)++] = (Beam) {{beam.origin.x + newDir1.x, beam.origin.y + newDir1.y}, newDir1};

                 Coord newDir2 = rotateNeg90(beam.dir);
                 nextBeams[(*numNextBeams)++] = (Beam) {{beam.origin.x + newDir2.x, beam.origin.y + newDir2.y}, newDir2};
             } else {
                 nextBeams[(*numNextBeams)++] = (Beam) {{beam.origin.x + beam.dir.x, beam.origin.y + beam.dir.y}, beam.dir};
             }
             break;
        }
        case '-': {
            if (beam.dir.y != 0) {
                Coord newDir1 = rotate90(beam.dir);
                nextBeams[(*numNextBeams)++] = (Beam) {{beam.origin.x + newDir1.x, beam.origin.y + newDir1.y}, newDir1};

                Coord newDir2 = rotateNeg90(beam.dir);
                nextBeams[(*numNextBeams)++] = (Beam) {{beam.origin.x + newDir2.x, beam.origin.y + newDir2.y}, newDir2};
             } else {
                nextBeams[(*numNextBeams)++] = (Beam) {{beam.origin.x + beam.dir.x, beam.origin.y + beam.dir.y}, beam.dir};
            }
            break;
        }
    }
}

int calculatePropagation(Grid grid, Beam start, bool visited[MAX_GRID_SIZE][MAX_GRID_SIZE][4]) {
    Beam toExplore[MAX_BEAMS];
    int toExploreSize = 0;
    toExplore[toExploreSize++] = start;

    int dx[] = {0, 1, 0, -1};
    int dy[] = {-1, 0, 1, 0};

    int energizedCount = 0;
    bool energized[MAX_GRID_SIZE][MAX_GRID_SIZE] = {false};

    while (toExploreSize > 0) {
        Beam currentBeam = toExplore[--toExploreSize];
        int dirIndex;

        for(int i = 0; i < 4; i++) {
            if(currentBeam.dir.x == dx[i] && currentBeam.dir.y == dy[i]) {
                dirIndex = i;
                break;
            }
        }

        if (!isInBounds(currentBeam.origin, grid) || visited[currentBeam.origin.y][currentBeam.origin.x][dirIndex]) continue;
        
        visited[currentBeam.origin.y][currentBeam.origin.x][dirIndex] = true;
       
        if(!energized[currentBeam.origin.y][currentBeam.origin.x]) {
            energized[currentBeam.origin.y][currentBeam.origin.x] = true;
            energizedCount++;
        }

        Beam nextBeams[4];
        int numNextBeams = 0;
        nextBeam(grid, currentBeam, nextBeams, &numNextBeams);
        for (int i = 0; i < numNextBeams; i++) {
            if(toExploreSize < MAX_BEAMS)
                toExplore[toExploreSize++] = nextBeams[i];
        }
    }

    return energizedCount;
}

int solve(char input[MAX_GRID_SIZE][MAX_GRID_SIZE], int numRows, int numCols) {
    Grid grid;
    buildGrid(&grid, input, numRows, numCols);
    Beam start = {{0, 0}, EAST};
    bool visited[MAX_GRID_SIZE][MAX_GRID_SIZE][4] = {false};
    return calculatePropagation(grid, start, visited);
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char input[MAX_GRID_SIZE][MAX_GRID_SIZE];
    int numRows = 0;
    int numCols = 0;

    while (fgets(input[numRows], MAX_GRID_SIZE, fp) != NULL) {
        input[numRows][strcspn(input[numRows], "\n")] = '\0';
        if(numRows == 0) numCols = strlen(input[numRows]);
        numRows++;
    }
    fclose(fp);

    printf("%d\n", solve(input, numRows, numCols));
    return 0;
}
