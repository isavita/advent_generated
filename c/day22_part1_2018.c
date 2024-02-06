
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int depth, target_x, target_y;
    fscanf(fp, "depth: %d\n", &depth);
    fscanf(fp, "target: %d,%d\n", &target_x, &target_y);

    int cave[target_x + 1][target_y + 1];
    int risk_level = 0;

    for (int x = 0; x <= target_x; x++) {
        for (int y = 0; y <= target_y; y++) {
            int geologic_index;
            if ((x == 0 && y == 0) || (x == target_x && y == target_y)) {
                geologic_index = 0;
            } else if (y == 0) {
                geologic_index = x * 16807;
            } else if (x == 0) {
                geologic_index = y * 48271;
            } else {
                geologic_index = cave[x-1][y] * cave[x][y-1];
            }
            cave[x][y] = (geologic_index + depth) % 20183;
            risk_level += cave[x][y] % 3;
        }
    }

    printf("%d\n", risk_level);

    fclose(fp);

    return 0;
}
