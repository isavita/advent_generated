
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_MESSAGES 1000
#define MAX_LENGTH 100

int getMostCommonChar(int count[128]) {
    int maxChar = 0;
    for (int i = 0; i < 128; ++i) {
        if (count[i] > count[maxChar]) {
            maxChar = i;
        }
    }
    return maxChar;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    char messages[MAX_MESSAGES][MAX_LENGTH];
    int numMessages = 0;
    while (fgets(messages[numMessages], MAX_LENGTH, file) != NULL) {
        messages[numMessages][strcspn(messages[numMessages], "\n")] = 0; // Remove newline
        numMessages++;
    }
    fclose(file);

    if (numMessages == 0) {
        printf("\n");
        return EXIT_SUCCESS;
    }

    int messageLength = strlen(messages[0]);
    int count[MAX_LENGTH][128] = {0}; // ASCII table size for simplicity

    for (int i = 0; i < numMessages; ++i) {
        for (int j = 0; j < messageLength; ++j) {
            count[j][(int)messages[i][j]]++;
        }
    }

    for (int i = 0; i < messageLength; ++i) {
        putchar(getMostCommonChar(count[i]));
    }
    printf("\n");

    return EXIT_SUCCESS;
}
