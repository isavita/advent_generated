
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *imageData = (char *)malloc(file_size + 1);
    if (imageData == NULL) {
        fclose(fp);
        perror("Memory allocation failed");
        return 1;
    }

    fread(imageData, 1, file_size, fp);
    fclose(fp);
    imageData[file_size] = '\0';

    int width = 25;
    int height = 6;
    int layerSize = width * height;
    char *finalImage = (char *)malloc(layerSize * sizeof(char));
    if (finalImage == NULL) {
         free(imageData);
        perror("Memory allocation failed");
        return 1;
    }
    
    for (int i = 0; i < layerSize; i++) {
        finalImage[i] = '2';
    }

    for (int i = 0; i < file_size; i += layerSize) {
        int layerEnd = (i + layerSize < file_size) ? (i + layerSize) : file_size;
        for (int j = 0; j < layerEnd - i; j++) {
            if (finalImage[j] == '2') {
                finalImage[j] = imageData[i + j];
            }
        }
    }
    printf("Decoded image:\n");
    for (int i = 0; i < height; i++) {
        for (int j = 0; j < width; j++) {
            char pixel = finalImage[i * width + j];
            if (pixel == '0') {
                printf(" ");
            } else if (pixel == '1') {
                printf("#");
            }
        }
        printf("\n");
    }

    free(imageData);
    free(finalImage);
    return 0;
}
