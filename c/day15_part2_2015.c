
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INGREDIENTS 10
#define MAX_NAME_LEN 50

typedef struct {
    char name[MAX_NAME_LEN];
    int capacity;
    int durability;
    int flavor;
    int texture;
    int calories;
} Ingredient;

int readIngredients(const char *filename, Ingredient ingredients[], int *count);
int findMaxScore(Ingredient ingredients[], int ingredientCount, int totalTeaspoons, int targetCalories);
int calculateMaxScore(Ingredient ingredients[], int ingredientCount, int index, int remaining, int teaspoons[], int targetCalories);
int score(Ingredient ingredients[], int teaspoons[], int ingredientCount);
int calculateCalories(Ingredient ingredients[], int teaspoons[], int ingredientCount);

int main(void) {
    Ingredient ingredients[MAX_INGREDIENTS];
    int ingredientCount = 0;

    if (!readIngredients("input.txt", ingredients, &ingredientCount)) {
        printf("Error reading input.\n");
        return 1;
    }

    int maxScore = findMaxScore(ingredients, ingredientCount, 100, 500);
    printf("%d\n", maxScore);

    return 0;
}

int readIngredients(const char *filename, Ingredient ingredients[], int *count) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        return 0;
    }

    while (fscanf(file, "%s capacity %d, durability %d, flavor %d, texture %d, calories %d",
                  ingredients[*count].name,
                  &ingredients[*count].capacity,
                  &ingredients[*count].durability,
                  &ingredients[*count].flavor,
                  &ingredients[*count].texture,
                  &ingredients[*count].calories) == 6) {
        (*count)++;
        if (*count >= MAX_INGREDIENTS) break;
    }

    fclose(file);
    return 1;
}

int findMaxScore(Ingredient ingredients[], int ingredientCount, int totalTeaspoons, int targetCalories) {
    int teaspoons[MAX_INGREDIENTS] = {0};
    return calculateMaxScore(ingredients, ingredientCount, 0, totalTeaspoons, teaspoons, targetCalories);
}

int calculateMaxScore(Ingredient ingredients[], int ingredientCount, int index, int remaining, int teaspoons[], int targetCalories) {
    if (index == ingredientCount - 1) {
        teaspoons[index] = remaining;
        if (calculateCalories(ingredients, teaspoons, ingredientCount) == targetCalories) {
            return score(ingredients, teaspoons, ingredientCount);
        }
        return 0;
    }

    int maxScore = 0;
    for (int i = 0; i <= remaining; i++) {
        teaspoons[index] = i;
        int currentScore = calculateMaxScore(ingredients, ingredientCount, index + 1, remaining - i, teaspoons, targetCalories);
        if (currentScore > maxScore) {
            maxScore = currentScore;
        }
    }
    return maxScore;
}

int score(Ingredient ingredients[], int teaspoons[], int ingredientCount) {
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

int calculateCalories(Ingredient ingredients[], int teaspoons[], int ingredientCount) {
    int calories = 0;
    for (int i = 0; i < ingredientCount; i++) {
        calories += ingredients[i].calories * teaspoons[i];
    }
    return calories;
}
