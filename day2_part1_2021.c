#include <stdio.h>

int main() {
    FILE *file = fopen("input.txt", "r");

    int horizontal = 0, depth = 0;
    char action[10];
    int value;

    while (fscanf(file, "%s %d", action, &value) != EOF) {
        if (action[0] == 'f') {
            horizontal += value;
        } else if (action[0] == 'd') {
            depth += value;
        } else if (action[0] == 'u') {
            depth -= value;
        }
    }

    fclose(file);

    printf("%d\n", horizontal * depth);

    return 0;
}