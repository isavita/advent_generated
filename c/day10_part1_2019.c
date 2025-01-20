
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

typedef struct {
    int x, y;
} Point;

int gcd(int a, int b) {
    while (b) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

int countVisibleAsteroids(bool **asteroids, int width, int height, int x, int y) {
    int count = 0;
    bool seen[2000][2000] = {false};
    for (int otherY = 0; otherY < height; otherY++) {
        for (int otherX = 0; otherX < width; otherX++) {
            if (asteroids[otherY][otherX] && !(otherX == x && otherY == y)) {
                int dx = otherX - x;
                int dy = otherY - y;
                int g = gcd(abs(dx), abs(dy));
                int sx = dx / g;
                int sy = dy / g;
                if(!seen[sx+1000][sy+1000]) {
                    seen[sx+1000][sy+1000]=true;
                    count++;
                }
            }
        }
    }
    return count;
}

int findBestAsteroidLocation(bool **asteroids, int width, int height) {
    int maxCount = 0;
    for (int y = 0; y < height; y++) {
        for (int x = 0; x < width; x++) {
            if (asteroids[y][x]) {
                int count = countVisibleAsteroids(asteroids, width, height, x, y);
                if (count > maxCount) {
                    maxCount = count;
                }
            }
        }
    }
    return maxCount;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[1024];
    int height = 0;
    int width = 0;
    while (fgets(line, sizeof(line), file) != NULL) {
        if(width==0){
            for (width = 0; line[width]!='\0' && line[width]!='\n'; width++);
        }
        height++;
    }

    fseek(file, 0, SEEK_SET);
    
    bool **asteroids = (bool **)malloc(height * sizeof(bool *));
    for (int i = 0; i < height; i++) {
        asteroids[i] = (bool *)malloc(width * sizeof(bool));
    }

    int row = 0;
    while (fgets(line, sizeof(line), file) != NULL) {
        for (int col = 0; col < width; col++) {
            asteroids[row][col] = line[col] == '#';
        }
        row++;
    }
    fclose(file);
    

    int maxCount = findBestAsteroidLocation(asteroids, width, height);
    printf("%d\n", maxCount);
    
    for (int i = 0; i < height; i++) {
        free(asteroids[i]);
    }
    free(asteroids);
    
    return 0;
}
