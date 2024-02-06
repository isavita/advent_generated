
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_REPLACEMENTS 100
#define MAX_MOLECULE_LENGTH 1000
#define MAX_LINE_LENGTH 1024

typedef struct {
    char key[50];
    char value[50];
} Replacement;

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Could not open file");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    Replacement replacements[MAX_REPLACEMENTS];
    int replacementsCount = 0;
    char molecule[MAX_MOLECULE_LENGTH] = {0};
    char *molecules[MAX_REPLACEMENTS * MAX_MOLECULE_LENGTH];
    int moleculesCount = 0;

    while (fgets(line, sizeof(line), file)) {
        if (strstr(line, " => ")) {
            char *token = strtok(line, " =>\n");
            strcpy(replacements[replacementsCount].key, token);
            token = strtok(NULL, " =>\n");
            strcpy(replacements[replacementsCount].value, token);
            replacementsCount++;
        } else if (strcmp(line, "\n") != 0) {
            line[strcspn(line, "\n")] = 0; // Remove newline character
            strcpy(molecule, line);
        }
    }
    fclose(file);

    for (int i = 0; i < replacementsCount; i++) {
        char *ptr = molecule;
        while ((ptr = strstr(ptr, replacements[i].key)) != NULL) {
            char newMolecule[MAX_MOLECULE_LENGTH];
            int index = ptr - molecule;
            snprintf(newMolecule, sizeof(newMolecule), "%.*s%s%s", index, molecule, replacements[i].value, ptr + strlen(replacements[i].key));
            int exists = 0;
            for (int j = 0; j < moleculesCount; j++) {
                if (strcmp(molecules[j], newMolecule) == 0) {
                    exists = 1;
                    break;
                }
            }
            if (!exists) {
                molecules[moleculesCount] = strdup(newMolecule);
                if (molecules[moleculesCount] == NULL) {
                    perror("Memory allocation failed");
                    return 1;
                }
                moleculesCount++;
            }
            ptr++;
        }
    }

    printf("%d\n", moleculesCount);

    for (int i = 0; i < moleculesCount; i++) {
        free(molecules[i]);
    }

    return 0;
}
