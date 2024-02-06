
#include <stdio.h>

int main() {
    int grid[100][100] = {0};
    int temp[100][100] = {0};

    FILE* file = fopen("input.txt", "r");
    for (int i = 0; i < 100; i++) {
        for (int j = 0; j < 100; j++) {
            char c;
            fscanf(file, " %c", &c);
            grid[i][j] = (c == '#') ? 1 : 0;
        }
    }
    fclose(file);

    for (int step = 0; step < 100; step++) {
        for (int i = 0; i < 100; i++) {
            for (int j = 0; j < 100; j++) {
                int count = 0;
                for (int dx = -1; dx <= 1; dx++) {
                    for (int dy = -1; dy <= 1; dy++) {
                        if (dx == 0 && dy == 0) continue;
                        int nx = i + dx;
                        int ny = j + dy;
                        if (nx < 0 || nx >= 100 || ny < 0 || ny >= 100) continue;
                        count += grid[nx][ny];
                    }
                }

                if (grid[i][j] == 1) {
                    temp[i][j] = (count == 2 || count == 3) ? 1 : 0;
                } else {
                    temp[i][j] = (count == 3) ? 1 : 0;
                }
            }
        }

        for (int i = 0; i < 100; i++) {
            for (int j = 0; j < 100; j++) {
                grid[i][j] = temp[i][j];
            }
        }
    }

    int result = 0;
    for (int i = 0; i < 100; i++) {
        for (int j = 0; j < 100; j++) {
            result += grid[i][j];
        }
    }

    printf("%d\n", result);

    return 0;
}
