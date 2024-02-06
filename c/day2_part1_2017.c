
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

int main() {
    FILE *file;
    char *filename = "input.txt";
    char line[1024];
    int checksum = 0;

    file = fopen(filename, "r");
    if (file == NULL) {
        printf("File reading error\n");
        return 1;
    }

    while (fgets(line, sizeof(line), file)) {
        int minVal = INT_MAX;
        int maxVal = INT_MIN;
        char *token = strtok(line, " \t\n");
        while (token != NULL) {
            int num = atoi(token);
            if (num < minVal) minVal = num;
            if (num > maxVal) maxVal = num;
            token = strtok(NULL, " \t\n");
        }
        checksum += (maxVal - minVal);
    }

    fclose(file);
    printf("%d\n", checksum);
    return 0;
}
