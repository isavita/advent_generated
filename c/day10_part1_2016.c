
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_BOTS 256
#define MAX_OUTPUTS 256

typedef struct {
    int low_type;  // 0 for bot, 1 for output
    int low_id;
    int high_type; // 0 for bot, 1 for output
    int high_id;
    int chips[2];
    int chip_count;
} Bot;

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    Bot bots[MAX_BOTS] = {0};
    int outputs[MAX_OUTPUTS] = {0};

    char line[128];
    while (fgets(line, sizeof(line), file)) {
        if (strncmp(line, "value", 5) == 0) {
            int value, bot_id;
            sscanf(line, "value %d goes to bot %d", &value, &bot_id);
            bots[bot_id].chips[bots[bot_id].chip_count++] = value;
        } else {
            int bot_id, low_id, high_id;
            char low_type[10], high_type[10];
            sscanf(line, "bot %d gives low to %s %d and high to %s %d",
                   &bot_id, low_type, &low_id, high_type, &high_id);
            bots[bot_id].low_type = (strcmp(low_type, "output") == 0);
            bots[bot_id].low_id = low_id;
            bots[bot_id].high_type = (strcmp(high_type, "output") == 0);
            bots[bot_id].high_id = high_id;
        }
    }
    fclose(file);
    
    int target_bot = -1;
    while (1) {
        int moved = 0;
        for (int i = 0; i < MAX_BOTS; i++) {
            if (bots[i].chip_count == 2) {
                moved = 1;
                int low = (bots[i].chips[0] < bots[i].chips[1]) ? bots[i].chips[0] : bots[i].chips[1];
                int high = (bots[i].chips[0] > bots[i].chips[1]) ? bots[i].chips[0] : bots[i].chips[1];

                if (low == 17 && high == 61) {
                    target_bot = i;
                }

                if (bots[i].low_type == 0) { // Give to bot
                    bots[bots[i].low_id].chips[bots[bots[i].low_id].chip_count++] = low;
                } else { // Give to output
                    outputs[bots[i].low_id] = low;
                }

                if (bots[i].high_type == 0) { // Give to bot
                    bots[bots[i].high_id].chips[bots[bots[i].high_id].chip_count++] = high;
                } else { // Give to output
                    outputs[bots[i].high_id] = high;
                }
                bots[i].chip_count = 0;
            }
        }
        if (!moved) break;
    }
    
    printf("%d\n", target_bot);

    return 0;
}
