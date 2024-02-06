
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error reading file\n");
        return 1;
    }

    char buffer[1024];
    fgets(buffer, sizeof(buffer), file);
    fclose(file);

    char *token = strtok(buffer, ",");
    int startingNumbers[1024];
    int count = 0;

    while (token != NULL) {
        startingNumbers[count++] = atoi(token);
        token = strtok(NULL, ",");
    }

    int lastSpoken[2020];
    for (int i = 0; i < 2020; i++) {
        lastSpoken[i] = -1;
    }

    int lastNumber, nextNumber;

    for (int turn = 1; turn <= 2020; turn++) {
        if (turn - 1 < count) {
            lastNumber = startingNumbers[turn - 1];
            lastSpoken[lastNumber] = turn;
            continue;
        }

        int lastTurn = lastSpoken[lastNumber];
        if (lastTurn != -1 && lastTurn != turn - 1) {
            nextNumber = turn - 1 - lastTurn;
        } else {
            nextNumber = 0;
        }

        lastSpoken[lastNumber] = turn - 1;
        lastNumber = nextNumber;
    }

    printf("%d\n", lastNumber);

    return 0;
}
