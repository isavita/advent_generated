
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file;
    char line[256];
    int sueNum, amount1, amount2, amount3;
    char thing1[20], thing2[20], thing3[20];
    int targetSue[10] = {3, 7, 2, 3, 0, 0, 5, 3, 2, 1}; // children, cats, samoyeds, pomeranians, akitas, vizslas, goldfish, trees, cars, perfumes

    file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    while (fgets(line, sizeof(line), file)) {
        if (sscanf(line, "Sue %d: %19s %d, %19s %d, %19s %d", &sueNum, thing1, &amount1, thing2, &amount2, thing3, &amount3) == 7) {
            // Clean up the read items
            thing1[strlen(thing1) - 1] = '\0'; // Remove the colon
            thing2[strlen(thing2) - 1] = '\0'; // Remove the colon
            thing3[strlen(thing3) - 1] = '\0'; // Remove the colon

            int allRulesMatched = 1;
            int counts[3] = {amount1, amount2, amount3};
            char *things[3] = {thing1, thing2, thing3};
            int indexes[3];

            // Convert string to index
            for (int i = 0; i < 3; i++) {
                if (strcmp(things[i], "children") == 0) indexes[i] = 0;
                else if (strcmp(things[i], "cats") == 0) indexes[i] = 1;
                else if (strcmp(things[i], "samoyeds") == 0) indexes[i] = 2;
                else if (strcmp(things[i], "pomeranians") == 0) indexes[i] = 3;
                else if (strcmp(things[i], "akitas") == 0) indexes[i] = 4;
                else if (strcmp(things[i], "vizslas") == 0) indexes[i] = 5;
                else if (strcmp(things[i], "goldfish") == 0) indexes[i] = 6;
                else if (strcmp(things[i], "trees") == 0) indexes[i] = 7;
                else if (strcmp(things[i], "cars") == 0) indexes[i] = 8;
                else if (strcmp(things[i], "perfumes") == 0) indexes[i] = 9;
                else indexes[i] = -1; // Not found
            }

            // Check rules
            for (int i = 0; i < 3; i++) {
                if (indexes[i] == -1) continue;

                if (indexes[i] == 1 || indexes[i] == 7) { // cats, trees
                    if (counts[i] <= targetSue[indexes[i]]) allRulesMatched = 0;
                } else if (indexes[i] == 3 || indexes[i] == 6) { // pomeranians, goldfish
                    if (counts[i] >= targetSue[indexes[i]]) allRulesMatched = 0;
                } else {
                    if (counts[i] != targetSue[indexes[i]]) allRulesMatched = 0;
                }
            }

            if (allRulesMatched) {
                printf("%d\n", sueNum);
                fclose(file);
                return EXIT_SUCCESS;
            }
        }
    }

    fclose(file);
    fprintf(stderr, "No matching Sue found.\n");
    return EXIT_FAILURE;
}
