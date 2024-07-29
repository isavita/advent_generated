#include <stdio.h>
#include <stdlib.h>

#define TOTAL_CUPS 1000000
#define TOTAL_MOVES 10000000

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) return 1;

    int cups[TOTAL_CUPS + 1] = {0};
    char input[100];
    fgets(input, sizeof(input), file);
    fclose(file);

    int lastCup = 0;
    for (int i = 0; input[i] != '\0' && input[i] != '\n'; i++) {
        int cup = input[i] - '0';
        if (i > 0) cups[lastCup] = cup;
        lastCup = cup;
    }

    for (int i = strlen(input) + 1; i <= TOTAL_CUPS; i++) {
        cups[lastCup] = i;
        lastCup = i;
    }
    cups[lastCup] = input[0] - '0';

    int currentCup = input[0] - '0';
    for (int i = 0; i < TOTAL_MOVES; i++) {
        int pickup1 = cups[currentCup], pickup2 = cups[pickup1], pickup3 = cups[pickup2];
        cups[currentCup] = cups[pickup3];

        int destinationCup = currentCup - 1;
        if (destinationCup == 0) destinationCup = TOTAL_CUPS;
        while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
            destinationCup--;
            if (destinationCup == 0) destinationCup = TOTAL_CUPS;
        }

        cups[pickup3] = cups[destinationCup];
        cups[destinationCup] = pickup1;

        currentCup = cups[currentCup];
    }

    long long cup1 = cups[1];
    long long cup2 = cups[cup1];
    printf("%lld\n", cup1 * cup2);
    return 0;
}