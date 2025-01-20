
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NODES 1000

typedef struct {
    int x;
    int y;
    int size;
    int used;
    int avail;
} Node;

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Failed to open input file");
        return 1;
    }

    Node nodes[MAX_NODES];
    int nodeCount = 0;

    char line[256];
    while (fgets(line, sizeof(line), file)) {
        if (sscanf(line, "/dev/grid/node-x%d-y%d %dT %dT %dT", &nodes[nodeCount].x, &nodes[nodeCount].y, &nodes[nodeCount].size, &nodes[nodeCount].used, &nodes[nodeCount].avail) == 5) {
            nodeCount++;
        }
    }
    fclose(file);

    int viablePairs = 0;
    for (int i = 0; i < nodeCount; i++) {
        for (int j = 0; j < nodeCount; j++) {
            if (i != j && nodes[i].used > 0 && nodes[i].used <= nodes[j].avail) {
                viablePairs++;
            }
        }
    }

    printf("Number of viable pairs: %d\n", viablePairs);

    return 0;
}
