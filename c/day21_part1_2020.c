
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LEN 1024
#define MAX_ITEMS 100 // Max ingredients or allergens per line
#define MAX_UNIQUE_INGREDIENTS 500
#define MAX_UNIQUE_ALLERGENS 50

typedef struct {
    char* name;
    int count;
    bool potentially_allergenic;
} Ingredient;

typedef struct {
    char* name;
    char* potential_ingredients[MAX_UNIQUE_INGREDIENTS];
    int potential_count;
} Allergen;

Ingredient ingredients[MAX_UNIQUE_INGREDIENTS];
int ingredient_count = 0;

Allergen allergens[MAX_UNIQUE_ALLERGENS];
int allergen_count = 0;

// Find ingredient index by name, return -1 if not found
int find_ingredient(const char* name) {
    for (int i = 0; i < ingredient_count; ++i) {
        if (strcmp(ingredients[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

// Add ingredient or increment count
int add_or_update_ingredient(const char* name) {
    int index = find_ingredient(name);
    if (index != -1) {
        ingredients[index].count++;
        return index;
    } else {
        if (ingredient_count >= MAX_UNIQUE_INGREDIENTS) {
            fprintf(stderr, "Error: Too many unique ingredients.\n");
            exit(1);
        }
        ingredients[ingredient_count].name = strdup(name);
        ingredients[ingredient_count].count = 1;
        ingredients[ingredient_count].potentially_allergenic = false; // Initialize
        return ingredient_count++;
    }
}

// Find allergen index by name, return -1 if not found
int find_allergen(const char* name) {
    for (int i = 0; i < allergen_count; ++i) {
        if (strcmp(allergens[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

// Check if an ingredient name exists in a list of potential ingredients
bool contains_ingredient_name(char* list[], int count, const char* name) {
    for (int i = 0; i < count; ++i) {
        if (strcmp(list[i], name) == 0) {
            return true;
        }
    }
    return false;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove trailing newline

        char *ingredients_part = strtok(line, "(");
        char *allergens_part = strtok(NULL, ")");

        if (!ingredients_part || !allergens_part) continue; // Skip malformed lines

        // --- Process Ingredients ---
        char *current_ingredients[MAX_ITEMS];
        int current_ingredient_count = 0;
        char *token = strtok(ingredients_part, " ");
        while (token != NULL) {
            if (strlen(token) > 0) {
                 add_or_update_ingredient(token);
                 current_ingredients[current_ingredient_count++] = strdup(token); // Store for current line processing
            }
            token = strtok(NULL, " ");
        }

        // --- Process Allergens ---
        // Skip "contains " prefix
         if (strncmp(allergens_part, "contains ", 9) == 0) {
             allergens_part += 9;
         } else {
            // Handle case where "contains " might be missing or different format
             char* actual_allergens = strstr(allergens_part, "contains ");
             if (actual_allergens) {
                 allergens_part = actual_allergens + 9;
             } else {
                 // If still not found, assume starting directly with allergens, trim leading spaces
                 while (*allergens_part == ' ') allergens_part++;
             }
         }


        token = strtok(allergens_part, ", ");
        while (token != NULL) {
            if (strlen(token) > 0) {
                int allergen_index = find_allergen(token);
                if (allergen_index == -1) {
                    // New allergen
                    if (allergen_count >= MAX_UNIQUE_ALLERGENS) {
                        fprintf(stderr, "Error: Too many unique allergens.\n");
                        // Cleanup allocated ingredient names for this line before exiting
                        for(int i = 0; i < current_ingredient_count; ++i) free(current_ingredients[i]);
                        exit(1);
                    }
                    allergen_index = allergen_count++;
                    allergens[allergen_index].name = strdup(token);
                    allergens[allergen_index].potential_count = 0;
                    // Add all current ingredients as potential candidates
                    for (int i = 0; i < current_ingredient_count; ++i) {
                         allergens[allergen_index].potential_ingredients[allergens[allergen_index].potential_count++] = strdup(current_ingredients[i]);
                    }
                } else {
                    // Existing allergen - perform intersection
                    int current_potential_count = allergens[allergen_index].potential_count;
                    int new_potential_count = 0;
                    char* new_potential_ingredients[MAX_UNIQUE_INGREDIENTS]; // Temp storage

                    for (int i = 0; i < current_potential_count; ++i) {
                        bool found_in_current_food = false;
                        for (int j = 0; j < current_ingredient_count; ++j) {
                            if (strcmp(allergens[allergen_index].potential_ingredients[i], current_ingredients[j]) == 0) {
                                found_in_current_food = true;
                                break;
                            }
                        }
                        if (found_in_current_food) {
                            // Keep this ingredient - copy pointer to temp list
                            new_potential_ingredients[new_potential_count++] = allergens[allergen_index].potential_ingredients[i];
                        } else {
                            // Discard this ingredient - free its memory
                            free(allergens[allergen_index].potential_ingredients[i]);
                        }
                    }
                     // Update the allergen's list
                    allergens[allergen_index].potential_count = new_potential_count;
                     for(int i=0; i < new_potential_count; ++i) {
                         allergens[allergen_index].potential_ingredients[i] = new_potential_ingredients[i];
                     }
                }
            }
            token = strtok(NULL, ", ");
        }
         // Free ingredient names duplicated for the current line
         for(int i = 0; i < current_ingredient_count; ++i) {
             free(current_ingredients[i]);
         }
    }
    fclose(file);

    // --- Identify potentially allergenic ingredients ---
    for (int i = 0; i < allergen_count; ++i) {
        for (int j = 0; j < allergens[i].potential_count; ++j) {
            int ing_idx = find_ingredient(allergens[i].potential_ingredients[j]);
            if (ing_idx != -1) {
                ingredients[ing_idx].potentially_allergenic = true;
            }
        }
    }

    // --- Count safe ingredients ---
    long long safe_ingredients_count = 0;
    for (int i = 0; i < ingredient_count; ++i) {
        if (!ingredients[i].potentially_allergenic) {
            safe_ingredients_count += ingredients[i].count;
        }
    }

    printf("%lld\n", safe_ingredients_count);

    // --- Cleanup ---
    for (int i = 0; i < ingredient_count; ++i) {
        free(ingredients[i].name);
    }
     for (int i = 0; i < allergen_count; ++i) {
         free(allergens[i].name);
         // Free remaining potential ingredient names within each allergen
         for (int j = 0; j < allergens[i].potential_count; ++j) {
              free(allergens[i].potential_ingredients[j]);
         }
     }


    return 0;
}
