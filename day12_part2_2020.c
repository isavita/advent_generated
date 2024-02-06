
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int x, y;
    int waypointX, waypointY;
} Ship;

void processInstruction(Ship *ship, char action, int value);
void rotateWaypoint(Ship *ship, int degrees);
int abs(int x);

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    Ship ship = {0, 0, 10, 1};
    char line[100];

    while (fgets(line, sizeof(line), file)) {
        char action = line[0];
        int value = atoi(line + 1);
        processInstruction(&ship, action, value);
    }

    fclose(file);

    int manhattanDistance = abs(ship.x) + abs(ship.y);
    printf("%d\n", manhattanDistance);

    return 0;
}

void processInstruction(Ship *ship, char action, int value) {
    switch (action) {
        case 'N':
            ship->waypointY += value;
            break;
        case 'S':
            ship->waypointY -= value;
            break;
        case 'E':
            ship->waypointX += value;
            break;
        case 'W':
            ship->waypointX -= value;
            break;
        case 'L':
            rotateWaypoint(ship, -value);
            break;
        case 'R':
            rotateWaypoint(ship, value);
            break;
        case 'F':
            ship->x += ship->waypointX * value;
            ship->y += ship->waypointY * value;
            break;
    }
}

void rotateWaypoint(Ship *ship, int degrees) {
    degrees = (degrees + 360) % 360;
    int temp;
    switch (degrees) {
        case 90:
        case -270:
            temp = ship->waypointX;
            ship->waypointX = ship->waypointY;
            ship->waypointY = -temp;
            break;
        case 180:
        case -180:
            ship->waypointX = -ship->waypointX;
            ship->waypointY = -ship->waypointY;
            break;
        case 270:
        case -90:
            temp = ship->waypointX;
            ship->waypointX = -ship->waypointY;
            ship->waypointY = temp;
            break;
    }
}

int abs(int x) {
    return x < 0 ? -x : x;
}
