
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STATES 100

struct Command {
    int write;
    int move;
    char nextState;
};

struct State {
    struct Command commands[2];
};

int parseInput(const char *filePath, char *initialState, int *steps, struct State states[MAX_STATES]) {
    FILE *file = fopen(filePath, "r");
    if (!file) {
        perror("Error opening file");
        exit(EXIT_FAILURE);
    }

    char line[100];
    fgets(line, sizeof(line), file);
    *initialState = line[strlen(line) - 3];

    fgets(line, sizeof(line), file);
    sscanf(line, "Perform a diagnostic checksum after %d steps.", steps);

    while (fgets(line, sizeof(line), file) != NULL) {
        if (strlen(line) <= 1) continue; // Skip empty lines

        char stateId;
        sscanf(line, "In state %c:", &stateId);

        for (int i = 0; i < 2; i++) {
            fgets(line, sizeof(line), file); // Read "If the current value is X:"

            int write;
            fgets(line, sizeof(line), file);
            sscanf(line, "    - Write the value %d.", &write);

            int move;
            fgets(line, sizeof(line), file);
            if (strstr(line, "right")) {
                move = 1;
            } else {
                move = -1;
            }

            char nextState;
            fgets(line, sizeof(line), file);
            sscanf(line, "    - Continue with state %c.", &nextState);

            states[stateId - 'A'].commands[i].write = write;
            states[stateId - 'A'].commands[i].move = move;
            states[stateId - 'A'].commands[i].nextState = nextState;
        }
    }

    fclose(file);
    return 0;
}

int runTuringMachine(const char *filePath) {
    char initialState;
    int steps;
    struct State states[MAX_STATES] = {0};

    parseInput(filePath, &initialState, &steps, states);

    int tape[10000] = {0}; // Simplified tape representation
    int cursor = 5000; // Start in the middle of the tape
    char state = initialState;
    int checksum = 0;

    for (int i = 0; i < steps; i++) {
        int value = tape[cursor];
        struct Command cmd = states[state - 'A'].commands[value];

        tape[cursor] = cmd.write;
        cursor += cmd.move;
        state = cmd.nextState;
    }

    for (int i = 0; i < 10000; i++) {
        checksum += tape[i];
    }

    return checksum;
}

int main() {
    int result = runTuringMachine("input.txt");
    printf("%d\n", result);
    return 0;
}
