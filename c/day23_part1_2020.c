#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CUPS 1000000

int cups[MAX_CUPS];

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    char input[100];
    fscanf(file, "%s", input);
    fclose(file);

    int currentCup = input[0] - '0';
    for (int i = 0; i < strlen(input); i++) {
        int cup = input[i] - '0';
        if (i < strlen(input) - 1) {
            cups[cup] = input[i + 1] - '0';
        }
    }
    cups[input[strlen(input) - 1] - '0'] = input[0] - '0';

    for (int i = 0; i < 100; i++) {
        int pickup1 = cups[currentCup];
        int pickup2 = cups[pickup1];
        int pickup3 = cups[pickup2];

        cups[currentCup] = cups[pickup3];

        int destinationCup = currentCup - 1;
        if (destinationCup < 1) {
            destinationCup = strlen(input);
        }
        while (destinationCup == pickup1 || destinationCup == pickup2 || destinationCup == pickup3) {
            destinationCup--;
            if (destinationCup < 1) {
                destinationCup = strlen(input);
            }
        }

        cups[pickup3] = cups[destinationCup];
        cups[destinationCup] = pickup1;

        currentCup = cups[currentCup];
    }

    int cup = cups[1];
    while (cup != 1) {
        printf("%d", cup);
        cup = cups[cup];
        if (cup == 1) {
            break;
        }
    }
    printf("\n");

    return 0;
}