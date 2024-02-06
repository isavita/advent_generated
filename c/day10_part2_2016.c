
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_BOTS 256
#define MAX_OUTPUTS 256

typedef struct {
    int lowTo, highTo;
    int chips[2];
    int chipCount;
} Bot;

typedef struct {
    Bot bots[MAX_BOTS];
    int outputs[MAX_OUTPUTS];
} Environment;

void giveChip(Environment *env, int target, int value, int isBot) {
    if (isBot) {
        Bot *bot = &env->bots[target];
        if (bot->chipCount < 2) {
            bot->chips[bot->chipCount++] = value;
        }
    } else {
        env->outputs[target] = value;
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    Environment env = {0};
    char line[256];
    while (fgets(line, sizeof(line), file)) {
        int value, botID, lowTo, highTo;
        char lowType[10], highType[10];
        if (sscanf(line, "value %d goes to bot %d", &value, &botID) == 2) {
            giveChip(&env, botID, value, 1);
        } else if (sscanf(line, "bot %d gives low to %s %d and high to %s %d", &botID, lowType, &lowTo, highType, &highTo) == 5) {
            Bot *bot = &env.bots[botID];
            bot->lowTo = (strcmp(lowType, "bot") == 0) ? lowTo : -1 - lowTo;
            bot->highTo = (strcmp(highType, "bot") == 0) ? highTo : -1 - highTo;
        }
    }
    fclose(file);

    int action;
    do {
        action = 0;
        for (int i = 0; i < MAX_BOTS; i++) {
            if (env.bots[i].chipCount == 2) {
                action = 1;
                int low = env.bots[i].chips[0] < env.bots[i].chips[1] ? env.bots[i].chips[0] : env.bots[i].chips[1];
                int high = env.bots[i].chips[0] > env.bots[i].chips[1] ? env.bots[i].chips[0] : env.bots[i].chips[1];
                giveChip(&env, env.bots[i].lowTo < 0 ? -1 - env.bots[i].lowTo : env.bots[i].lowTo, low, env.bots[i].lowTo >= 0);
                giveChip(&env, env.bots[i].highTo < 0 ? -1 - env.bots[i].highTo : env.bots[i].highTo, high, env.bots[i].highTo >= 0);
                env.bots[i].chipCount = 0;
            }
        }
    } while (action);

    printf("%d\n", env.outputs[0] * env.outputs[1] * env.outputs[2]);
    return EXIT_SUCCESS;
}
