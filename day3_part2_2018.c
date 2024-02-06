
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int ID;
    int X;
    int Y;
    int Width;
    int Height;
} Claim;

Claim* readClaims(const char* filename, int* count) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        perror("Unable to open file");
        exit(1);
    }

    Claim* claims = NULL;
    *count = 0;
    char line[256];
    while (fgets(line, sizeof(line), file)) {
        Claim claim;
        sscanf(line, "#%d @ %d,%d: %dx%d", &claim.ID, &claim.X, &claim.Y, &claim.Width, &claim.Height);
        claims = realloc(claims, (*count + 1) * sizeof(Claim));
        claims[*count] = claim;
        (*count)++;
    }

    fclose(file);
    return claims;
}

int main() {
    int count;
    Claim* claims = readClaims("input.txt", &count);

    int fabric[1000][1000] = {0};

    for (int i = 0; i < count; i++) {
        for (int y = claims[i].Y; y < claims[i].Y + claims[i].Height; y++) {
            for (int x = claims[i].X; x < claims[i].X + claims[i].Width; x++) {
                fabric[y][x]++;
            }
        }
    }

    for (int i = 0; i < count; i++) {
        int overlap = 0;
        for (int y = claims[i].Y; y < claims[i].Y + claims[i].Height && !overlap; y++) {
            for (int x = claims[i].X; x < claims[i].X + claims[i].Width; x++) {
                if (fabric[y][x] > 1) {
                    overlap = 1;
                    break;
                }
            }
        }
        if (!overlap) {
            printf("%d\n", claims[i].ID);
            break;
        }
    }

    free(claims);
    return 0;
}
