
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define WIDTH 101
#define HEIGHT 103
#define MAX_ROBOTS 1000

typedef struct {
    int x, y, vx, vy;
} Robot;

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    Robot robots[MAX_ROBOTS];
    int robot_count = 0;
    char line[256];

    while (fgets(line, sizeof(line), file) && robot_count < MAX_ROBOTS) {
        int px, py, vx, vy;
        sscanf(line, "p=%d,%d v=%d,%d", &px, &py, &vx, &vy);
        robots[robot_count].x = px;
        robots[robot_count].y = py;
        robots[robot_count].vx = vx;
        robots[robot_count].vy = vy;
        robot_count++;
    }
    fclose(file);

    for (int i = 0; i < 100; i++) {
        for (int j = 0; j < robot_count; j++) {
            robots[j].x = (robots[j].x + robots[j].vx) % WIDTH;
            robots[j].y = (robots[j].y + robots[j].vy) % HEIGHT;
            if (robots[j].x < 0) robots[j].x += WIDTH;
            if (robots[j].y < 0) robots[j].y += HEIGHT;
        }
    }

    int q1 = 0, q2 = 0, q3 = 0, q4 = 0;
    for (int i = 0; i < robot_count; i++) {
        int x = robots[i].x;
        int y = robots[i].y;
        if (x == 50 || y == 51) continue;
        if (x < 50 && y < 51) q1++;
        else if (x > 50 && y < 51) q2++;
        else if (x < 50 && y > 51) q3++;
        else if (x > 50 && y > 51) q4++;
    }

    printf("%d\n", q1 * q2 * q3 * q4);

    return 0;
}
