
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int x, y, facing;
} Ship;

void processInstruction(Ship *ship, char action, int value) {
    switch (action) {
        case 'N':
            ship->y += value;
            break;
        case 'S':
            ship->y -= value;
            break;
        case 'E':
            ship->x += value;
            break;
        case 'W':
            ship->x -= value;
            break;
        case 'L':
            ship->facing = (ship->facing - value + 360) % 360;
            break;
        case 'R':
            ship->facing = (ship->facing + value) % 360;
            break;
        case 'F':
            switch (ship->facing) {
                case 0:
                    ship->x += value;
                    break;
                case 90:
                    ship->y -= value;
                    break;
                case 180:
                    ship->x -= value;
                    break;
                case 270:
                    ship->y += value;
                    break;
            }
            break;
    }
}

int abs(int x) {
    return x < 0 ? -x : x;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    Ship ship = {0, 0, 0};
    char line[100];
    while (fgets(line, sizeof(line), file)) {
        char action = line[0];
        int value = atoi(&line[1]);
        processInstruction(&ship, action, value);
    }

    int manhattanDistance = abs(ship.x) + abs(ship.y);
    printf("%d\n", manhattanDistance);

    fclose(file);
    return 0;
}
