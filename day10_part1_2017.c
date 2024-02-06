
#include <stdio.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("File reading error");
        return 1;
    }

    int lengths[100];
    int length;
    int i = 0;
    while (fscanf(file, "%d,", &length) != EOF) {
        lengths[i] = length;
        i++;
    }
    fclose(file);

    int list[256];
    for (int i = 0; i < 256; i++) {
        list[i] = i;
    }
    int currentPosition = 0;
    int skipSize = 0;

    for (int j = 0; j < i; j++) {
        int length = lengths[j];
        for (int k = 0; k < length/2; k++) {
            int start = (currentPosition + k) % 256;
            int end = (currentPosition + length - 1 - k) % 256;
            int temp = list[start];
            list[start] = list[end];
            list[end] = temp;
        }
        currentPosition = (currentPosition + length + skipSize) % 256;
        skipSize++;
    }

    int result = list[0] * list[1];
    printf("%d\n", result);

    return 0;
}
