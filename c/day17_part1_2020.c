
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_SIZE 50
#define CYCLES 6

typedef struct {
    int x, y, z;
} Coordinate;

typedef struct {
    Coordinate coords[MAX_SIZE * MAX_SIZE * MAX_SIZE];
    int count;
} ActiveCubes;

bool isActive(ActiveCubes *cubes, Coordinate coord) {
    for (int i = 0; i < cubes->count; i++) {
        if (cubes->coords[i].x == coord.x && cubes->coords[i].y == coord.y && cubes->coords[i].z == coord.z) {
            return true;
        }
    }
    return false;
}

void addActiveCube(ActiveCubes *cubes, Coordinate coord) {
    cubes->coords[cubes->count++] = coord;
}


ActiveCubes simulateCycle(ActiveCubes *activeCubes) {
    ActiveCubes newActiveCubes = {0};
    int neighborCounts[2 * MAX_SIZE][2 * MAX_SIZE][2 * MAX_SIZE] = {0};
    int minX = MAX_SIZE, maxX = -MAX_SIZE, minY = MAX_SIZE, maxY = -MAX_SIZE, minZ = MAX_SIZE, maxZ = -MAX_SIZE;
    
    for(int i = 0; i < activeCubes->count; i++){
      Coordinate coord = activeCubes->coords[i];
      if (coord.x < minX) minX = coord.x;
      if (coord.x > maxX) maxX = coord.x;
      if (coord.y < minY) minY = coord.y;
      if (coord.y > maxY) maxY = coord.y;
      if (coord.z < minZ) minZ = coord.z;
      if (coord.z > maxZ) maxZ = coord.z;

        for (int dz = -1; dz <= 1; dz++) {
            for (int dy = -1; dy <= 1; dy++) {
                for (int dx = -1; dx <= 1; dx++) {
                    if (dx == 0 && dy == 0 && dz == 0) continue;
                    int nx = coord.x + dx + MAX_SIZE /2;
                    int ny = coord.y + dy + MAX_SIZE /2;
                    int nz = coord.z + dz + MAX_SIZE /2;
                    neighborCounts[nx][ny][nz]++;
                }
            }
        }
    }
    
    for (int z = minZ -1; z <= maxZ+1; z++){
       for (int y = minY-1; y <= maxY+1; y++){
         for(int x = minX -1; x <= maxX+1; x++){
            int nx = x + MAX_SIZE/2;
            int ny = y + MAX_SIZE/2;
            int nz = z + MAX_SIZE/2;
           int count = neighborCounts[nx][ny][nz];
           Coordinate coord = {x,y,z};
            if (count == 3 || (count == 2 && isActive(activeCubes, coord))) {
                addActiveCube(&newActiveCubes, coord);
            }
          }
       }
    }
    return newActiveCubes;
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_SIZE];
    ActiveCubes activeCubes = {0};
    int y = 0;

    while (fgets(line, sizeof(line), fp) != NULL) {
        for (int x = 0; line[x] != '\0' && line[x] != '\n'; x++) {
            if (line[x] == '#') {
                Coordinate coord = {x, y, 0};
                addActiveCube(&activeCubes, coord);
            }
        }
        y++;
    }
    fclose(fp);

    for (int cycle = 0; cycle < CYCLES; cycle++) {
        activeCubes = simulateCycle(&activeCubes);
    }
    printf("%d\n", activeCubes.count);

    return 0;
}
