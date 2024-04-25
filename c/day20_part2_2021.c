#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ITERATIONS 50
#define EXPAND_BY 1

char* readAlgorithm(FILE* file) {
    char* algorithm = malloc(513);
    fgets(algorithm, 513, file);
    algorithm[strcspn(algorithm, "\n")] = 0;
    return algorithm;
}

int** readImage(FILE* file, int* height, int* width) {
    fscanf(file, "\n");
    *height = 0;
    *width = 0;
    int** image = NULL;
    char line[513];
    while (fgets(line, 513, file)) {
        int len = strlen(line);
        if (line[len - 1] == '\n') {
            line[--len] = 0;
        }
        if (*width == 0) {
            *width = len;
        }
        image = realloc(image, (*height + 1) * sizeof(int*));
        image[*height] = malloc(*width * sizeof(int));
        for (int i = 0; i < *width; i++) {
            image[*height][i] = line[i] == '#';
        }
        (*height)++;
    }
    return image;
}

int** enhanceImage(char* algorithm, int** image, int height, int width, int useInfiniteLit) {
    int** newImage = malloc((height + 2 * EXPAND_BY) * sizeof(int*));
    for (int i = 0; i < height + 2 * EXPAND_BY; i++) {
        newImage[i] = malloc((width + 2 * EXPAND_BY) * sizeof(int));
    }
    for (int y = -EXPAND_BY; y < height + EXPAND_BY; y++) {
        for (int x = -EXPAND_BY; x < width + EXPAND_BY; x++) {
            int index = 0;
            for (int dy = -1; dy <= 1; dy++) {
                for (int dx = -1; dx <= 1; dx++) {
                    index <<= 1;
                    int ny = y + dy, nx = x + dx;
                    if (ny >= 0 && ny < height && nx >= 0 && nx < width) {
                        if (image[ny][nx]) {
                            index |= 1;
                        }
                    } else if (useInfiniteLit) {
                        index |= 1;
                    }
                }
            }
            newImage[y + EXPAND_BY][x + EXPAND_BY] = algorithm[index] == '#';
        }
    }
    return newImage;
}

int countLitPixels(int** image, int height, int width) {
    int count = 0;
    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            if (image[i][j]) {
                count++;
            }
        }
    }
    return count;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    char* algorithm = readAlgorithm(file);
    int height, width;
    int** image = readImage(file, &height, &width);
    for (int i = 0; i < ITERATIONS; i++) {
        image = enhanceImage(algorithm, image, height, width, i % 2 == 1 && algorithm[0] == '#');
        height += 2 * EXPAND_BY;
        width += 2 * EXPAND_BY;
    }
    printf("%d\n", countLitPixels(image, height, width));
    return 0;
}