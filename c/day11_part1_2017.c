
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int abs_val(int x) {
    return x < 0 ? -x : x;
}

int max_val(int a, int b) {
    return a > b ? a : b;
}

int distance(int x, int y, int z) {
    return (abs_val(x) + abs_val(y) + abs_val(z)) / 2;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    if ((read = getline(&line, &len, file)) == -1) {
        perror("Error reading line");
        fclose(file);
        return 1;
    }

    fclose(file);

    int x = 0, y = 0, z = 0;
    int maxDistance = 0;
    char *token = strtok(line, ",");

    while (token != NULL) {
        if (strcmp(token, "n") == 0) {
            y++;
            z--;
        } else if (strcmp(token, "ne") == 0) {
            x++;
            z--;
        } else if (strcmp(token, "se") == 0) {
            x++;
            y--;
        } else if (strcmp(token, "s") == 0) {
            y--;
            z++;
        } else if (strcmp(token, "sw") == 0) {
            x--;
            z++;
        } else if (strcmp(token, "nw") == 0) {
            x--;
            y++;
        }
        maxDistance = max_val(maxDistance, distance(x, y, z));
        token = strtok(NULL, ",");
    }
    printf("%d\n", distance(x, y, z));
    free(line);
    return 0;
}
