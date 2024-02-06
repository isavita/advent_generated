
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void swapPositions(char *pw, int x, int y) {
    char temp = pw[x];
    pw[x] = pw[y];
    pw[y] = temp;
}

void swapLetters(char *pw, char x, char y) {
    char *px = strchr(pw, x);
    char *py = strchr(pw, y);
    if (px && py) {
        swapPositions(pw, px - pw, py - pw);
    }
}

void rotate(char *pw, int steps) {
    int length = strlen(pw);
    steps = steps % length;
    if (steps < 0) {
        steps += length;
    }
    char *temp = (char *)malloc(length + 1);
    strcpy(temp, pw + length - steps);
    strncat(temp, pw, length - steps);
    strcpy(pw, temp);
    free(temp);
}

void rotateLetter(char *pw, char x) {
    char *px = strchr(pw, x);
    int index = px ? px - pw : -1;
    if (index >= 4) {
        index++;
    }
    rotate(pw, index + 1);
}

void derotateLetter(char *pw, char x) {
    int index = strchr(pw, x) - pw;
    int rot;
    if (index % 2 == 1) {
        rot = -(index + 1) / 2;
    } else if (index != 0) {
        rot = (6 - index) / 2;
    } else {
        rot = -1;
    }
    rotate(pw, rot);
}

void reverse(char *pw, int x, int y) {
    while (x < y) {
        swapPositions(pw, x++, y--);
    }
}

void move(char *pw, int x, int y) {
    char ch = pw[x];
    if (x < y) {
        memmove(pw + x, pw + x + 1, y - x);
    } else {
        memmove(pw + y + 1, pw + y, x - y);
    }
    pw[y] = ch;
}

void unscramble(char *pw, char **instructions, int count) {
    for (int i = count - 1; i >= 0; i--) {
        char *instr = instructions[i];
        if (strncmp(instr, "swap position", 13) == 0) {
            int x, y;
            sscanf(instr, "swap position %d with position %d", &x, &y);
            swapPositions(pw, x, y);
        } else if (strncmp(instr, "swap letter", 11) == 0) {
            char x, y;
            sscanf(instr, "swap letter %c with letter %c", &x, &y);
            swapLetters(pw, x, y);
        } else if (strncmp(instr, "rotate based", 12) == 0) {
            char x;
            sscanf(instr, "rotate based on position of letter %c", &x);
            derotateLetter(pw, x);
        } else if (strncmp(instr, "rotate", 6) == 0) {
            char dir[5];
            int steps;
            sscanf(instr, "rotate %s %d", dir, &steps);
            if (strcmp(dir, "left") == 0) steps = -steps;
            rotate(pw, -steps);
        } else if (strncmp(instr, "reverse", 7) == 0) {
            int x, y;
            sscanf(instr, "reverse positions %d through %d", &x, &y);
            reverse(pw, x, y);
        } else if (strncmp(instr, "move", 4) == 0) {
            int x, y;
            sscanf(instr, "move position %d to position %d", &x, &y);
            move(pw, y, x); // reversed for unscrambling
        }
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Could not open file");
        return 1;
    }

    char *instructions[1000];
    char line[256];
    int count = 0;

    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0; // Remove newline
        instructions[count] = strdup(line);
        count++;
    }
    fclose(file);

    char pw[] = "fbgdceah";
    unscramble(pw, instructions, count);

    printf("%s\n", pw);

    for (int i = 0; i < count; i++) {
        free(instructions[i]);
    }

    return 0;
}
