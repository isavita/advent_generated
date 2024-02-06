
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int x, y;
} Point;

int abs(int x) {
    return x < 0 ? -x : x;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("File could not be opened.\n");
        return 1;
    }

    Point head = {0, 0}, tail = {0, 0};
    int visited[10000][2]; // Assuming a max of 10000 points visited for simplicity
    int visitedCount = 0;
    visited[visitedCount][0] = tail.x;
    visited[visitedCount][1] = tail.y;
    visitedCount++;

    char direction[2];
    int steps;
    while (fscanf(file, "%s %d", direction, &steps) != EOF) {
        for (int i = 0; i < steps; i++) {
            if (strcmp(direction, "R") == 0) head.x++;
            else if (strcmp(direction, "L") == 0) head.x--;
            else if (strcmp(direction, "U") == 0) head.y++;
            else if (strcmp(direction, "D") == 0) head.y--;

            if (abs(head.x - tail.x) > 1 || abs(head.y - tail.y) > 1) {
                if (head.x != tail.x && head.y != tail.y) {
                    if (head.x > tail.x) tail.x++;
                    else tail.x--;

                    if (head.y > tail.y) tail.y++;
                    else tail.y--;
                } else {
                    if (head.x > tail.x) tail.x++;
                    else if (head.x < tail.x) tail.x--;

                    if (head.y > tail.y) tail.y++;
                    else if (head.y < tail.y) tail.y--;
                }
            }

            int found = 0;
            for (int j = 0; j < visitedCount; j++) {
                if (visited[j][0] == tail.x && visited[j][1] == tail.y) {
                    found = 1;
                    break;
                }
            }

            if (!found) {
                visited[visitedCount][0] = tail.x;
                visited[visitedCount][1] = tail.y;
                visitedCount++;
            }
        }
    }

    fclose(file);

    printf("%d\n", visitedCount);
    return 0;
}
