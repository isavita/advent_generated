
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>
#include <ctype.h>

int calculateNewFuel(int currentPosition, int newPosition) {
    int diff = abs(currentPosition - newPosition);
    return (diff * (diff + 1)) / 2;
}

int abs(int n) {
    return n < 0 ? -n : n;
}

int compare(const void *a, const void *b) {
    return (*(int *)a - *(int *)b);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    int positions[2000];
    int num_positions = 0;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    while ((read = getline(&line, &len, file)) != -1) {
        char *token = strtok(line, ",");
        while (token != NULL) {
             int num = 0;
             for(int i = 0; token[i] != '\0'; i++){
                 if(isdigit(token[i])){
                    num = num * 10 + (token[i] - '0');
                 }
             }
            positions[num_positions++] = num;
            token = strtok(NULL, ",");
        }
    }
    free(line);
    fclose(file);

    qsort(positions, num_positions, sizeof(int), compare);

    int min_fuel = INT_MAX;
    int min_pos = positions[0];
    int max_pos = positions[num_positions - 1];
    
    for (int i = min_pos; i <= max_pos; i++) {
        int fuel = 0;
        for (int j = 0; j < num_positions; j++) {
            fuel += calculateNewFuel(positions[j], i);
        }
        if (fuel < min_fuel) {
            min_fuel = fuel;
        }
    }

    printf("%d\n", min_fuel);
    return 0;
}
