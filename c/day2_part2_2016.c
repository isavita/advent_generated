
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Function to get the next position on the keypad
char* getNextPosition(char* position, char move) {
    static char nextPos[2];
    nextPos[1] = '\0';

    switch (position[0]) {
        case '1': if (move == 'D') return "3"; break;
        case '2': if (move == 'R') return "3"; else if (move == 'D') return "6"; break;
        case '3': if (move == 'U') return "1"; else if (move == 'R') return "4"; else if (move == 'D') return "7"; else if (move == 'L') return "2"; break;
        case '4': if (move == 'L') return "3"; else if (move == 'D') return "8"; break;
        case '5': if (move == 'R') return "6"; break;
        case '6': if (move == 'U') return "2"; else if (move == 'R') return "7"; else if (move == 'D') return "A"; else if (move == 'L') return "5"; break;
        case '7': if (move == 'U') return "3"; else if (move == 'R') return "8"; else if (move == 'D') return "B"; else if (move == 'L') return "6"; break;
        case '8': if (move == 'U') return "4"; else if (move == 'R') return "9"; else if (move == 'D') return "C"; else if (move == 'L') return "7"; break;
        case '9': if (move == 'L') return "8"; break;
        case 'A': if (move == 'U') return "6"; else if (move == 'R') return "B"; break;
        case 'B': if (move == 'U') return "7"; else if (move == 'R') return "C"; else if (move == 'D') return "D"; else if (move == 'L') return "A"; break;
        case 'C': if (move == 'U') return "8"; else if (move == 'L') return "B"; break;
        case 'D': if (move == 'U') return "B"; break;
    }
    nextPos[0] = position[0]; // Stay in the same position if no valid move
    return nextPos;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Could not open file");
        return EXIT_FAILURE;
    }

    char *position = "5"; // Start at '5'
    char *nextPosition;
    char instruction[1024];
    char code[1024] = ""; // Assuming the code won't exceed this length

    while (fgets(instruction, sizeof(instruction), file)) {
        char *pos;
        instruction[strcspn(instruction, "\n")] = 0; // Remove newline character
        for (pos = instruction; *pos; ++pos) { // Iterate through each character in the instruction
            nextPosition = getNextPosition(position, *pos);
            position = nextPosition;
        }
        strncat(code, position, 1); // Append the position to the code
    }

    fclose(file);

    printf("%s\n", code);
    return EXIT_SUCCESS;
}
