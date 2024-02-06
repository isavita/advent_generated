#include <stdio.h>
#include <stdlib.h>

#define MAX_COMPONENTS 100
#define MAX_LENGTH 100

typedef struct {
    int port1;
    int port2;
    int used;
} Component;

Component components[MAX_COMPONENTS];
int numComponents = 0;
int maxLength = 0;
int maxStrength = 0;

void readInput() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening the file.\n");
        exit(1);
    }

    while (fscanf(file, "%d/%d", &components[numComponents].port1, &components[numComponents].port2) != EOF) {
        components[numComponents].used = 0;
        numComponents++;
    }

    fclose(file);
}

void buildBridge(int port, int length, int strength) {
    if (length > maxLength || (length == maxLength && strength > maxStrength)) {
        maxLength = length;
        maxStrength = strength;
    }

    for (int i = 0; i < numComponents; i++) {
        if (components[i].used == 0 && (components[i].port1 == port || components[i].port2 == port)) {
            components[i].used = 1;
            buildBridge(components[i].port1 == port ? components[i].port2 : components[i].port1, length + 1, strength + components[i].port1 + components[i].port2);
            components[i].used = 0;
        }
    }
}

int main() {
    readInput();
    buildBridge(0, 0, 0);

    printf("%d\n", maxStrength);

    return 0;
}