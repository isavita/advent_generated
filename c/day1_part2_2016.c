
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    int x, y;
} Position;

int firstRevisitedDistance(char instructions[][10], int size) {
    Position pos = {0, 0};
    int visited[1000][1000] = {0};
    visited[pos.x + 500][pos.y + 500] = 1;
    Position directions[4] = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
    int dirIndex = 0;

    for (int i = 0; i < size; i++) {
        char turn = instructions[i][0];
        int blocks = atoi(&instructions[i][1]);

        if (turn == 'R') {
            dirIndex = (dirIndex + 1) % 4;
        } else {
            dirIndex = (dirIndex - 1 + 4) % 4;
        }

        for (int j = 0; j < blocks; j++) {
            pos.x += directions[dirIndex].x;
            pos.y += directions[dirIndex].y;

            if (visited[pos.x + 500][pos.y + 500] == 1) {
                return abs(pos.x) + abs(pos.y);
            }
            visited[pos.x + 500][pos.y + 500] = 1;
        }
    }

    return -1;
}

int abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char instructions[1000][10];
    int size = 0;
    while (fscanf(file, "%s", instructions[size]) != EOF) {
        size++;
    }

    fclose(file);

    printf("%d\n", firstRevisitedDistance(instructions, size));

    return 0;
}
