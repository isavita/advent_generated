
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *imageData = malloc(fileSize + 1);
    fread(imageData, 1, fileSize, file);
    fclose(file);

    int width = 25;
    int height = 6;
    int layerSize = width * height;

    int minZeros = layerSize + 1;
    int result = 0;

    for (int i = 0; i < strlen(imageData); i += layerSize) {
        char layer[layerSize + 1];
        strncpy(layer, imageData + i, layerSize);
        layer[layerSize] = '\0';

        int zeroCount = 0, oneCount = 0, twoCount = 0;

        for (int j = 0; j < strlen(layer); j++) {
            switch (layer[j]) {
                case '0':
                    zeroCount++;
                    break;
                case '1':
                    oneCount++;
                    break;
                case '2':
                    twoCount++;
                    break;
            }
        }

        if (zeroCount < minZeros) {
            minZeros = zeroCount;
            result = oneCount * twoCount;
        }
    }

    printf("%d\n", result);

    return 0;
}
