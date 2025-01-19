
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

int abs(int x) {
    return x < 0 ? -x : x;
}

int max(int a, int b) {
    return a > b ? a : b;
}

int distance(int x, int y, int z) {
    return (abs(x) + abs(y) + abs(z)) / 2;
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
        fclose(file);
        if (line) free(line);
        perror("Error reading line");
        return 1;
    }
    
    fclose(file);

    int x = 0, y = 0, z = 0;
    int maxDistance = 0;
    char *token;
    char *saveptr;

    token = strtok_r(line, ",", &saveptr);
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

        maxDistance = max(maxDistance, distance(x, y, z));
        token = strtok_r(NULL, ",", &saveptr);
    }

    printf("%d\n", maxDistance);
    free(line);

    return 0;
}
