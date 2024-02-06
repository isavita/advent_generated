
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    int keypad[3][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    int x = 1;
    int y = 1;

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        exit(EXIT_FAILURE);
    }

    while ((read = getline(&line, &len, fp)) != -1) {
        for (int i = 0; i < read; i++) {
            if (line[i] == 'U' && y > 0) y--;
            if (line[i] == 'D' && y < 2) y++;
            if (line[i] == 'L' && x > 0) x--;
            if (line[i] == 'R' && x < 2) x++;
        }
        printf("%d", keypad[y][x]);
    }

    fclose(fp);
    if (line) {
        free(line);
    }
    return 0;
}
