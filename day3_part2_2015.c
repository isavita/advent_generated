
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int x = 0, y = 0;
    int grid[1000][1000] = {0};
    int santa_x = 500, santa_y = 500;
    int robo_x = 500, robo_y = 500;
    int santa_turn = 1;

    grid[santa_x][santa_y] = 1;

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        exit(EXIT_FAILURE);
    }

    while ((read = getline(&line, &len, fp)) != -1) {
        for (int i = 0; i < read; i++) {
            if (santa_turn) {
                switch(line[i]) {
                    case '^':
                        santa_y++;
                        break;
                    case 'v':
                        santa_y--;
                        break;
                    case '>':
                        santa_x++;
                        break;
                    case '<':
                        santa_x--;
                        break;
                }
                grid[santa_x][santa_y] = 1;
            } else {
                switch(line[i]) {
                    case '^':
                        robo_y++;
                        break;
                    case 'v':
                        robo_y--;
                        break;
                    case '>':
                        robo_x++;
                        break;
                    case '<':
                        robo_x--;
                        break;
                }
                grid[robo_x][robo_y] = 1;
            }
            santa_turn = !santa_turn;
        }
    }

    int count = 0;
    for (int i = 0; i < 1000; i++) {
        for (int j = 0; j < 1000; j++) {
            if (grid[i][j] == 1) {
                count++;
            }
        }
    }

    printf("%d\n", count);

    fclose(fp);
    if (line) {
        free(line);
    }
    exit(EXIT_SUCCESS);
}
