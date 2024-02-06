
#include <stdio.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    int counts[12][2] = {0};

    char num[100];
    while (fscanf(file, "%s", num) != EOF) {
        for (int i = 0; num[i] != '\0'; i++) {
            counts[i][num[i] - '0']++;
        }
    }
    fclose(file);

    int gammaRate = 0;
    int epsilonRate = 0;
    for (int i = 0; i < 12; i++) {
        if (counts[i][0] > counts[i][1]) {
            gammaRate |= 1 << (12 - i - 1);
        } else {
            epsilonRate |= 1 << (12 - i - 1);
        }
    }

    printf("%d\n", gammaRate * epsilonRate);

    return 0;
}
