
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LENGTH 200
#define MAX_GRID_SIZE 20000
#define MAX_PARTS 2000

typedef struct {
    int xmin, xmax, y, n;
} Part;

typedef struct {
    int x, y;
} Point;

Point Neighbors8[] = {
    {0, 1}, {0, -1}, {1, 0}, {-1, 0},
    {-1, -1}, {-1, 1}, {1, -1}, {1, 1}
};

bool isValidPart(Part p, char* grid, int gridWidth, int gridHeight) {
    for (int x = p.xmin; x <= p.xmax; x++) {
        for (int i = 0; i < 8; i++) {
            int nx = x + Neighbors8[i].x;
            int ny = p.y + Neighbors8[i].y;
            if (nx >= 0 && nx < gridWidth && ny >= 0 && ny < gridHeight) {
                char c = grid[ny * gridWidth + nx];
                if (c != '.' && (c < '0' || c > '9')) {
                    return true;
                }
            }
        }
    }
    return false;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char* grid = (char*)malloc(MAX_GRID_SIZE * sizeof(char));
    char line[MAX_LINE_LENGTH];
    int gridWidth = 0, gridHeight = 0;
    int gridIndex = 0;
    Part parts[MAX_PARTS];
    int partsCount = 0;
    Part* curr = NULL;

    while (fgets(line, MAX_LINE_LENGTH, file) != NULL) {
        int len = strlen(line);
        if (len > 0 && line[len-1] == '\n') {
           line[--len] = '\0';
        }
        if(curr != NULL){
            parts[partsCount++] = *curr;
            curr = NULL;
        }

        if (gridWidth == 0) gridWidth = len;

        for (int x = 0; x < len; x++) {
            char c = line[x];
            grid[gridIndex++] = c;
            if (c >= '0' && c <= '9') {
                if (curr == NULL) {
                    curr = (Part*)malloc(sizeof(Part));
                    curr->y = gridHeight;
                    curr->xmin = x;
                    curr->xmax = x;
                    curr->n = c - '0';
                } else {
                    curr->n = curr->n * 10 + (c - '0');
                    curr->xmax = x;
                }
            } else if (curr != NULL) {
                parts[partsCount++] = *curr;
                free(curr);
                curr = NULL;
            }
        }
        gridHeight++;
    }
     if (curr != NULL) {
        parts[partsCount++] = *curr;
        free(curr);
    }


    int* partsGrid = (int*)malloc(MAX_GRID_SIZE * sizeof(int));
    for(int i=0; i < MAX_GRID_SIZE; i++) partsGrid[i] = -1;

    for (int i = 0; i < partsCount; i++) {
        for (int x = parts[i].xmin; x <= parts[i].xmax; x++) {
             partsGrid[parts[i].y * gridWidth + x] = i;
        }
    }


    int sum = 0;
    for (int y = 0; y < gridHeight; y++) {
        for (int x = 0; x < gridWidth; x++) {
            if (grid[y * gridWidth + x] == '*') {
                int neighborParts[MAX_PARTS] ;
                for(int i=0;i<MAX_PARTS; i++) neighborParts[i] = -1;
                int neighborCount = 0;

                for (int i = 0; i < 8; i++) {
                    int nx = x + Neighbors8[i].x;
                    int ny = y + Neighbors8[i].y;
                    if (nx >= 0 && nx < gridWidth && ny >= 0 && ny < gridHeight) {
                        int partIndex = partsGrid[ny * gridWidth + nx];
                        if(partIndex != -1) {
                           bool found = false;
                            for(int j=0; j<neighborCount; j++){
                                if(neighborParts[j] == partIndex){
                                    found = true;
                                    break;
                                }
                            }
                            if(!found) neighborParts[neighborCount++] = partIndex;
                        }
                    }
                }
                if (neighborCount == 2) {
                    int prod = 1;
                    for(int i=0; i< neighborCount; i++){
                        prod *= parts[neighborParts[i]].n;
                    }
                    sum += prod;
                }
            }
        }
    }


    printf("%d\n", sum);
    free(grid);
    free(partsGrid);
    fclose(file);
    return 0;
}
