
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("File reading error\n");
        return 1;
    }

    char buffer[255];
    fgets(buffer, 255, file);
    int steps = atoi(buffer);
    fclose(file);

    int currentPos = 0;
    int valueAfterZero = 0;

    for (int i = 1; i <= 50000000; i++) {
        currentPos = (currentPos + steps) % i;
        if (currentPos == 0) {
            valueAfterZero = i;
        }
        currentPos++;
    }

    printf("%d\n", valueAfterZero);

    return 0;
}
