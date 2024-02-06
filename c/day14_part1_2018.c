#include <stdio.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    int input;
    fscanf(file, "%d", &input);
    fclose(file);

    int scoreboard[1000000] = {3, 7};
    int num_recipes = 2;
    int elf1 = 0;
    int elf2 = 1;

    while (num_recipes < input + 10) {
        int sum = scoreboard[elf1] + scoreboard[elf2];
        if (sum >= 10) {
            scoreboard[num_recipes++] = 1;
            scoreboard[num_recipes++] = sum % 10;
        } else {
            scoreboard[num_recipes++] = sum;
        }

        elf1 = (elf1 + 1 + scoreboard[elf1]) % num_recipes;
        elf2 = (elf2 + 1 + scoreboard[elf2]) % num_recipes;
    }

    printf("%d%d%d%d%d%d%d%d%d%d\n", scoreboard[input], scoreboard[input+1], scoreboard[input+2], scoreboard[input+3], scoreboard[input+4], scoreboard[input+5], scoreboard[input+6], scoreboard[input+7], scoreboard[input+8], scoreboard[input+9]);
    
    return 0;
}