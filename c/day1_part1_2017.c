
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("File reading error\n");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *input = malloc(fileSize + 1);
    fread(input, 1, fileSize, file);
    input[fileSize] = '\0';

    int sum = 0;

    for (int i = 0; i < fileSize; i++) {
        int next = (i + 1) % fileSize;
        if (input[i] == input[next]) {
            sum += input[i] - '0';
        }
    }

    printf("%d\n", sum);

    fclose(file);
    free(input);

    return 0;
}
