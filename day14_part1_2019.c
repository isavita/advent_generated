
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char name[20];
    int amount;
} Chemical;

typedef struct {
    Chemical output;
    Chemical inputs[10];
    int inputCount;
} Reaction;

Reaction reactions[100];
int reactionCount = 0;
int surplus[100] = {0};

int findReactionIndex(char *name) {
    for (int i = 0; i < reactionCount; i++) {
        if (strcmp(reactions[i].output.name, name) == 0) {
            return i;
        }
    }
    return -1;
}

Chemical parseChemical(char *s) {
    Chemical chem;
    sscanf(s, "%d %s", &chem.amount, chem.name);
    return chem;
}

int calculateOre(char *chem, int amount, int surplus[]) {
    if (strcmp(chem, "ORE") == 0) {
        return amount;
    }

    int index = findReactionIndex(chem);
    if (index == -1) {
        printf("Chemical not found: %s\n", chem);
        exit(1);
    }

    if (surplus[index] >= amount) {
        surplus[index] -= amount;
        return 0;
    }

    amount -= surplus[index];
    surplus[index] = 0;
    Reaction reaction = reactions[index];
    int times = (amount + reaction.output.amount - 1) / reaction.output.amount;
    int ore = 0;

    for (int i = 0; i < reaction.inputCount; i++) {
        ore += calculateOre(reaction.inputs[i].name, reaction.inputs[i].amount * times, surplus);
    }

    surplus[index] += times * reaction.output.amount - amount;
    return ore;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[256];
    while (fgets(line, sizeof(line), file)) {
        char *token = strtok(line, "=>");
        char inputs[100];
        strcpy(inputs, token);
        token = strtok(NULL, "=>");
        Chemical output = parseChemical(token);

        Reaction reaction;
        reaction.output = output;
        reaction.inputCount = 0;

        char *inputToken = strtok(inputs, ",");
        while (inputToken != NULL) {
            reaction.inputs[reaction.inputCount++] = parseChemical(inputToken);
            inputToken = strtok(NULL, ",");
        }

        reactions[reactionCount++] = reaction;
    }
    fclose(file);

    printf("%d\n", calculateOre("FUEL", 1, surplus));
    return 0;
}
