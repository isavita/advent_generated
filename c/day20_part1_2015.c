#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    int input;
    fscanf(fp, "%d", &input);
    fclose(fp);

    int houses[1000000] = {0};

    for (int i = 1; i < 1000000; i++) {
        for (int j = i; j < 1000000; j += i) {
            houses[j] += i * 10;
        }
    }

    int result = 0;
    for (int i = 1; i < 1000000; i++) {
        if (houses[i] >= input) {
            result = i;
            break;
        }
    }

    printf("%d", result);

    return 0;
}