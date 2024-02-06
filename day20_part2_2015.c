
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char input[100];
    fgets(input, 100, file);
    fclose(file);

    int target = atoi(input) / 11;

    int *houses = (int *)malloc((target + 1) * sizeof(int));
    for (int i = 0; i <= target; i++) {
        houses[i] = 0;
    }

    for (int elf = 1; elf <= target; elf++) {
        for (int house = elf; house <= elf * 50 && house <= target; house += elf) {
            houses[house] += elf;
        }
    }

    for (int i = 0; i <= target; i++) {
        if (houses[i] >= target) {
            printf("%d\n", i);
            break;
        }
    }

    free(houses);
    return 0;
}
