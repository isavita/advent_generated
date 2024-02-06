
#include <stdio.h>

int countTrees(char forest[323][32], int right, int down) {
    int trees = 0;
    int x = 0;
    int width = 31;

    for (int y = 0; y < 323; y += down) {
        if (forest[y][x % width] == '#') {
            trees++;
        }
        x += right;
    }

    return trees;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    char forest[323][32];

    for (int i = 0; i < 323; i++) {
        fscanf(file, "%s", forest[i]);
    }

    int trees = countTrees(forest, 3, 1);
    printf("%d\n", trees);

    fclose(file);
    return 0;
}
