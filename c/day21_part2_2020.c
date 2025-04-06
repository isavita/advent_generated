
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LEN 1024
#define MAX_ITEMS 100 // Max ingredients or allergens per line
#define MAX_TOTAL_INGREDIENTS 1000
#define MAX_TOTAL_ALLERGENS 50
#define MAX_NAME_LEN 50

// --- Data Structures ---

typedef struct {
    char name[MAX_NAME_LEN];
    int count;
} IngredientCount;

typedef struct {
    char **ingredients;
    int count;
    int capacity;
} IngredientList;

typedef struct {
    char name[MAX_NAME_LEN];
    IngredientList candidates;
    char assigned_ingredient[MAX_NAME_LEN]; // For part 2
    bool assigned;
} AllergenInfo;

// Global storage
IngredientCount ingredient_counts[MAX_TOTAL_INGREDIENTS];
int total_unique_ingredients = 0;

AllergenInfo allergens[MAX_TOTAL_ALLERGENS];
int total_allergens = 0;

// --- Helper Functions ---

// Find or add ingredient to global count list
int find_or_add_ingredient(const char *name) {
    for (int i = 0; i < total_unique_ingredients; ++i) {
        if (strcmp(ingredient_counts[i].name, name) == 0) {
            ingredient_counts[i].count++;
            return i;
        }
    }
    if (total_unique_ingredients < MAX_TOTAL_INGREDIENTS) {
        strncpy(ingredient_counts[total_unique_ingredients].name, name, MAX_NAME_LEN - 1);
        ingredient_counts[total_unique_ingredients].name[MAX_NAME_LEN - 1] = '\0';
        ingredient_counts[total_unique_ingredients].count = 1;
        return total_unique_ingredients++;
    }
    fprintf(stderr, "Error: Exceeded MAX_TOTAL_INGREDIENTS\n");
    exit(1);
}

// Find or add allergen to global list
int find_or_add_allergen(const char *name) {
    for (int i = 0; i < total_allergens; ++i) {
        if (strcmp(allergens[i].name, name) == 0) {
            return i;
        }
    }
    if (total_allergens < MAX_TOTAL_ALLERGENS) {
        strncpy(allergens[total_allergens].name, name, MAX_NAME_LEN - 1);
        allergens[total_allergens].name[MAX_NAME_LEN - 1] = '\0';
        allergens[total_allergens].candidates.ingredients = NULL;
        allergens[total_allergens].candidates.count = 0;
        allergens[total_allergens].candidates.capacity = 0;
        allergens[total_allergens].assigned = false;
        allergens[total_allergens].assigned_ingredient[0] = '\0';
        return total_allergens++;
    }
    fprintf(stderr, "Error: Exceeded MAX_TOTAL_ALLERGENS\n");
    exit(1);
}

// Add ingredient to a dynamic list
void add_to_list(IngredientList *list, const char *ingredient) {
    if (list->count >= list->capacity) {
        list->capacity = (list->capacity == 0) ? 8 : list->capacity * 2;
        list->ingredients = (char **)realloc(list->ingredients, list->capacity * sizeof(char *));
        if (!list->ingredients) {
            perror("realloc failed");
            exit(1);
        }
    }
    list->ingredients[list->count] = strdup(ingredient);
     if (!list->ingredients[list->count]) {
        perror("strdup failed");
        exit(1);
    }
    list->count++;
}

// Free memory associated with an IngredientList
void free_list(IngredientList *list) {
    if (list->ingredients) {
        for (int i = 0; i < list->count; ++i) {
            free(list->ingredients[i]);
        }
        free(list->ingredients);
    }
    list->ingredients = NULL;
    list->count = 0;
    list->capacity = 0;
}

// Intersect current allergen candidates with a new list of ingredients
void intersect_candidates(AllergenInfo *allergen, char **current_ingredients, int current_count) {
    IngredientList *candidates = &allergen->candidates;
    IngredientList intersection;
    intersection.ingredients = NULL;
    intersection.count = 0;
    intersection.capacity = 0;

    for (int i = 0; i < candidates->count; ++i) {
        bool found = false;
        for (int j = 0; j < current_count; ++j) {
            if (strcmp(candidates->ingredients[i], current_ingredients[j]) == 0) {
                found = true;
                break;
            }
        }
        if (found) {
            add_to_list(&intersection, candidates->ingredients[i]);
        }
    }

    free_list(candidates);
    *candidates = intersection; // Transfer ownership
}

// Remove an ingredient from an allergen's candidate list
bool remove_candidate(AllergenInfo *allergen, const char *ingredient_to_remove) {
    IngredientList *candidates = &allergen->candidates;
    int found_idx = -1;
    for (int i = 0; i < candidates->count; ++i) {
        if (strcmp(candidates->ingredients[i], ingredient_to_remove) == 0) {
            found_idx = i;
            break;
        }
    }

    if (found_idx != -1) {
        free(candidates->ingredients[found_idx]);
        // Shift remaining elements left
        for (int i = found_idx; i < candidates->count - 1; ++i) {
            candidates->ingredients[i] = candidates->ingredients[i + 1];
        }
        candidates->count--;
        // Optional: shrink capacity if desired, but usually not critical
        return true; // Removed
    }
    return false; // Not found
}

// Comparison function for sorting allergens alphabetically
int compare_allergens(const void *a, const void *b) {
    return strcmp(((AllergenInfo *)a)->name, ((AllergenInfo *)b)->name);
}


// --- Main Logic ---

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (!fp) {
        perror("Error opening input.txt");
        return 1;
    }

    char line[MAX_LINE_LEN];
    char *line_ingredients[MAX_ITEMS];
    char *line_allergens[MAX_ITEMS];

    while (fgets(line, sizeof(line), fp)) {
        line[strcspn(line, "\n")] = 0; // Remove newline

        char *ingredients_part = strtok(line, "(");
        char *allergens_part = strtok(NULL, ")");
        if (!allergens_part) continue; // Should have allergens
        // Skip "contains "
        if (strncmp(allergens_part, "contains ", 9) == 0) {
            allergens_part += 9;
        }


        // Parse ingredients
        int current_ingredient_count = 0;
        char *token = strtok(ingredients_part, " ");
        while (token != NULL && current_ingredient_count < MAX_ITEMS) {
            find_or_add_ingredient(token); // Update global count
            line_ingredients[current_ingredient_count++] = token;
            token = strtok(NULL, " ");
        }

        // Parse allergens
        int current_allergen_count = 0;
        token = strtok(allergens_part, ", ");
         while (token != NULL && current_allergen_count < MAX_ITEMS) {
            line_allergens[current_allergen_count++] = token;
            token = strtok(NULL, ", ");
        }

        // Update allergen candidate lists
        for (int i = 0; i < current_allergen_count; ++i) {
            int allergen_idx = find_or_add_allergen(line_allergens[i]);
            AllergenInfo *current_allergen = &allergens[allergen_idx];

            if (current_allergen->candidates.ingredients == NULL) { // First time seeing this allergen
                 current_allergen->candidates.capacity = current_ingredient_count;
                 current_allergen->candidates.ingredients = (char **)malloc(current_allergen->candidates.capacity * sizeof(char *));
                 if (!current_allergen->candidates.ingredients) { perror("malloc failed"); exit(1); }
                 for(int j=0; j< current_ingredient_count; ++j){
                     current_allergen->candidates.ingredients[j] = strdup(line_ingredients[j]);
                     if (!current_allergen->candidates.ingredients[j]) { perror("strdup failed"); exit(1); }
                 }
                 current_allergen->candidates.count = current_ingredient_count;

            } else {
                intersect_candidates(current_allergen, line_ingredients, current_ingredient_count);
            }
        }
    }
    fclose(fp);

    // --- Part 1: Calculate safe ingredient count ---
    long long safe_count = 0;
    for (int i = 0; i < total_unique_ingredients; ++i) {
        bool is_potential_allergen = false;
        for (int j = 0; j < total_allergens; ++j) {
            for (int k = 0; k < allergens[j].candidates.count; ++k) {
                if (strcmp(ingredient_counts[i].name, allergens[j].candidates.ingredients[k]) == 0) {
                    is_potential_allergen = true;
                    break;
                }
            }
            if (is_potential_allergen) break;
        }
        if (!is_potential_allergen) {
            safe_count += ingredient_counts[i].count;
        }
    }
    printf("%lld\n", safe_count);

    // --- Part 2: Determine dangerous ingredients ---
    int assigned_count = 0;
    while (assigned_count < total_allergens) {
        bool changed = false;
        for (int i = 0; i < total_allergens; ++i) {
            if (!allergens[i].assigned && allergens[i].candidates.count == 1) {
                const char *ingredient = allergens[i].candidates.ingredients[0];
                strncpy(allergens[i].assigned_ingredient, ingredient, MAX_NAME_LEN -1);
                allergens[i].assigned_ingredient[MAX_NAME_LEN -1] = '\0';
                allergens[i].assigned = true;
                assigned_count++;
                changed = true;

                // Remove this ingredient from other allergens' candidate lists
                for (int j = 0; j < total_allergens; ++j) {
                    if (i != j && !allergens[j].assigned) {
                       remove_candidate(&allergens[j], ingredient);
                    }
                }
                 // No need to free the single ingredient string here, it's now 'owned' by assigned_ingredient conceptually
                 // but we still need to free the list pointer later
            }
        }
        if (!changed && assigned_count < total_allergens) {
             fprintf(stderr, "Error: Could not resolve all allergens.\n");
             // Free memory before exiting on error
             for(int i=0; i<total_allergens; ++i) free_list(&allergens[i].candidates);
             return 1; // Should not happen based on puzzle description
        }
    }

    // Sort allergens alphabetically
    qsort(allergens, total_allergens, sizeof(AllergenInfo), compare_allergens);

    // Print canonical dangerous ingredients list
    for (int i = 0; i < total_allergens; ++i) {
        printf("%s%s", allergens[i].assigned_ingredient, (i == total_allergens - 1) ? "" : ",");
    }
    printf("\n");


    // --- Cleanup ---
     for (int i = 0; i < total_allergens; ++i) {
         free_list(&allergens[i].candidates); // Free candidate lists including the single ones
     }
    // Note: ingredient_counts names are not dynamically allocated in this version

    return 0;
}
