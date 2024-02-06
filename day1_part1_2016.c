#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        printf("Error opening file\n");
        exit(1);
    }

    int x = 0, y = 0;
    char dir = 'N';
    char turn;
    int steps;
    
    while (fscanf(fp, "%c%d, ", &turn, &steps) != EOF) {
        if (turn == 'R') {
            if (dir == 'N') {
                dir = 'E';
            } else if (dir == 'E') {
                dir = 'S';
            } else if (dir == 'S') {
                dir = 'W';
            } else {
                dir = 'N';
            }
        } else {
            if (dir == 'N') {
                dir = 'W';
            } else if (dir == 'E') {
                dir = 'N';
            } else if (dir == 'S') {
                dir = 'E';
            } else {
                dir = 'S';
            }
        }
        
        if (dir == 'N') {
            y += steps;
        } else if (dir == 'S') {
            y -= steps;
        } else if (dir == 'E') {
            x += steps;
        } else {
            x -= steps;
        }
    }
    
    printf("%d\n", abs(x) + abs(y));

    fclose(fp);
    return 0;
}