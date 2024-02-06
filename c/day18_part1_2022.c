
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    int x, y, z;
    int surface_area = 0;
    int grid[100][100][100] = {0};

    while (fscanf(file, "%d,%d,%d\n", &x, &y, &z) != EOF) {
        grid[x][y][z] = 1;
    }

    fclose(file);

    for (int i = 0; i < 100; i++) {
        for (int j = 0; j < 100; j++) {
            for (int k = 0; k < 100; k++) {
                if (grid[i][j][k] == 1) {
                    surface_area += 6;
                    if (i > 0 && grid[i - 1][j][k] == 1) surface_area--;
                    if (i < 99 && grid[i + 1][j][k] == 1) surface_area--;
                    if (j > 0 && grid[i][j - 1][k] == 1) surface_area--;
                    if (j < 99 && grid[i][j + 1][k] == 1) surface_area--;
                    if (k > 0 && grid[i][j][k - 1] == 1) surface_area--;
                    if (k < 99 && grid[i][j][k + 1] == 1) surface_area--;
                }
            }
        }
    }

    printf("%d\n", surface_area);

    return 0;
}
