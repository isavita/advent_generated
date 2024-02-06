
#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        return 1;
    }

    int horizontal = 0;
    int depth = 0;
    int aim = 0;

    char action[10];
    int value;

    while (fscanf(fp, "%s %d\n", action, &value) != EOF) {
        if (action[0] == 'f') {
            horizontal += value;
            depth += aim * value;
        } else if (action[0] == 'd') {
            aim += value;
        } else if (action[0] == 'u') {
            aim -= value;
        }
    }

    fclose(fp);

    printf("%d\n", horizontal * depth);

    return 0;
}
