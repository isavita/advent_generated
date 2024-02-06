
#include <stdio.h>

typedef struct {
    int a, b;
} Component;

int maxStrength = 0;

void findStrongestBridge(Component components[], int used[], int port, int strength, int size) {
    if (strength > maxStrength) {
        maxStrength = strength;
    }

    for (int i = 0; i < size; i++) {
        if (used[i]) {
            continue;
        }

        if (components[i].a == port || components[i].b == port) {
            used[i] = 1;
            int nextPort = components[i].a;
            if (components[i].a == port) {
                nextPort = components[i].b;
            }
            findStrongestBridge(components, used, nextPort, strength + components[i].a + components[i].b, size);
            used[i] = 0;
        }
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Component components[100];
    int index = 0;
    while (fscanf(file, "%d/%d", &components[index].a, &components[index].b) == 2) {
        index++;
    }

    int used[100] = {0};
    findStrongestBridge(components, used, 0, 0, index);

    printf("%d\n", maxStrength);

    fclose(file);
    return 0;
}
