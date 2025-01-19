
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>

typedef struct {
    bool isOn;
    int x1, x2;
    int y1, y2;
    int z1, z2;
} Cube;

int max(int a, int b) {
    return a > b ? a : b;
}

int min(int a, int b) {
    return a < b ? a : b;
}

long long cubeVolume(Cube c) {
    long long vol = (long long)(c.x2 - c.x1 + 1) * (c.y2 - c.y1 + 1) * (c.z2 - c.z1 + 1);
    return c.isOn ? vol : -vol;
}

bool getIntersection(Cube c1, Cube c2, Cube *intersection) {
    int x1 = max(c1.x1, c2.x1);
    int x2 = min(c1.x2, c2.x2);
    int y1 = max(c1.y1, c2.y1);
    int y2 = min(c1.y2, c2.y2);
    int z1 = max(c1.z1, c2.z1);
    int z2 = min(c1.z2, c2.z2);

    if (x1 > x2 || y1 > y2 || z1 > z2) {
        return false;
    }
    
    bool intersectionState;
    if(c1.isOn && c2.isOn) {
        intersectionState = false;
    }else if (!c1.isOn && !c2.isOn){
        intersectionState = true;
    }else {
        intersectionState = c2.isOn;
    }


    *intersection = (Cube){intersectionState, x1, x2, y1, y2, z1, z2};
    return true;
}

Cube* parseInput(char *input, int* cubeCount) {
    *cubeCount = 0;
    int capacity = 10;
    Cube* cubes = malloc(capacity * sizeof(Cube));
    if (!cubes) {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }

    char *line = strtok(input, "\n");
    while (line != NULL) {
      
        char onOff[4];
        int x1, x2, y1, y2, z1, z2;
        if(sscanf(line, "%3s x=%d..%d,y=%d..%d,z=%d..%d",onOff, &x1, &x2, &y1, &y2, &z1, &z2) != 7){
             fprintf(stderr,"Parsing error in line: %s", line);
            exit(EXIT_FAILURE);
        }
         if (x1 > x2 || y1 > y2 || z1 > z2) {
            fprintf(stderr,"Invalid range in line: %s\n", line);
            exit(EXIT_FAILURE);
        }

        if (*cubeCount == capacity) {
            capacity *= 2;
            cubes = realloc(cubes, capacity * sizeof(Cube));
            if (!cubes) {
                perror("Memory allocation failed");
                exit(EXIT_FAILURE);
            }
        }
    
       cubes[*cubeCount] = (Cube){strcmp(onOff, "on") == 0, x1, x2, y1, y2, z1, z2};
        (*cubeCount)++;
        line = strtok(NULL, "\n");
    }
    return cubes;
}

long long solve(Cube* cubes, int cubeCount) {
    Cube *finalList = NULL;
    int finalCount = 0;
    int finalCapacity = 10;

    finalList = malloc(finalCapacity * sizeof(Cube));
     if (!finalList) {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < cubeCount; i++) {
        Cube c = cubes[i];
        Cube *toAdd = NULL;
        int toAddCount = 0;
        int toAddCapacity = 10;

         toAdd = malloc(toAddCapacity * sizeof(Cube));
         if (!toAdd) {
             perror("Memory allocation failed");
             exit(EXIT_FAILURE);
         }


        for (int j = 0; j < finalCount; j++) {
            Cube intersection;
           if (getIntersection(finalList[j], c, &intersection)) {
                if(toAddCount == toAddCapacity){
                     toAddCapacity *= 2;
                    toAdd = realloc(toAdd, toAddCapacity * sizeof(Cube));
                     if (!toAdd) {
                        perror("Memory allocation failed");
                        exit(EXIT_FAILURE);
                     }
                }
                toAdd[toAddCount++] = intersection;
            }
        }

         if (c.isOn) {
            if(toAddCount == toAddCapacity){
                    toAddCapacity *= 2;
                    toAdd = realloc(toAdd, toAddCapacity * sizeof(Cube));
                     if (!toAdd) {
                        perror("Memory allocation failed");
                        exit(EXIT_FAILURE);
                     }
                }
                toAdd[toAddCount++] = c;
            }

        for(int k = 0; k < toAddCount; k++) {
            if(finalCount == finalCapacity){
                 finalCapacity *= 2;
                  finalList = realloc(finalList, finalCapacity * sizeof(Cube));
                   if (!finalList) {
                        perror("Memory allocation failed");
                        exit(EXIT_FAILURE);
                    }
            }
           finalList[finalCount++] = toAdd[k];
        }
        free(toAdd);
    }

    long long total = 0;
    for (int i = 0; i < finalCount; i++) {
        total += cubeVolume(finalList[i]);
    }
    free(finalList);
    return total;
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *input = malloc(file_size + 1);
    if (!input) {
        perror("Memory allocation failed");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fread(input, 1, file_size, fp);
    fclose(fp);
    input[file_size] = '\0';

    int cubeCount;
    Cube* cubes = parseInput(input, &cubeCount);
    long long result = solve(cubes, cubeCount);

    printf("%lld\n", result);
    free(cubes);
    free(input);

    return EXIT_SUCCESS;
}
