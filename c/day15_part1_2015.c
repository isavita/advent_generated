
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char name[100];
    int capacity;
    int durability;
    int flavor;
    int texture;
} Ingredient;

Ingredient ingredients[10]; // Assuming a maximum of 10 ingredients
int ingredientCount = 0;

void readIngredients(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        printf("Error opening file\n");
        exit(1);
    }

    while (!feof(file)) {
        Ingredient ingredient;
        char line[200];
        if (fgets(line, sizeof(line), file)) {
            sscanf(line, "%s %*s %d, %*s %d, %*s %d, %*s %d",
                   ingredient.name,
                   &ingredient.capacity,
                   &ingredient.durability,
                   &ingredient.flavor,
                   &ingredient.texture);
            ingredients[ingredientCount++] = ingredient;
        }
    }

    fclose(file);
}

int score(int teaspoons[]) {
    int capacity = 0, durability = 0, flavor = 0, texture = 0;
    for (int i = 0; i < ingredientCount; i++) {
        capacity += ingredients[i].capacity * teaspoons[i];
        durability += ingredients[i].durability * teaspoons[i];
        flavor += ingredients[i].flavor * teaspoons[i];
        texture += ingredients[i].texture * teaspoons[i];
    }

    if (capacity < 0) capacity = 0;
    if (durability < 0) durability = 0;
    if (flavor < 0) flavor = 0;
    if (texture < 0) texture = 0;

    return capacity * durability * flavor * texture;
}

int calculateMaxScore(int index, int remaining, int teaspoons[], int depth) {
    if (index == ingredientCount - 1) {
        teaspoons[index] = remaining;
        return score(teaspoons);
    }

    int maxScore = 0;
    for (int i = 0; i <= remaining; i++) {
        teaspoons[index] = i;
        int currentScore = calculateMaxScore(index + 1, remaining - i, teaspoons, depth + 1);
        if (currentScore > maxScore) {
            maxScore = currentScore;
        }
    }
    return maxScore;
}

int findMaxScore(int totalTeaspoons) {
    int teaspoons[10] = {0}; // Assuming a maximum of 10 ingredients
    return calculateMaxScore(0, totalTeaspoons, teaspoons, 0);
}

int main() {
    readIngredients("input.txt");

    int maxScore = findMaxScore(100);
    printf("%d\n", maxScore);

    return 0;
}
