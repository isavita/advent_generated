
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void swapPosition(char *password, int x, int y) {
    char temp = password[x];
    password[x] = password[y];
    password[y] = temp;
}

void swapLetter(char *password, char x, char y) {
    for (int i = 0; password[i] != '\0'; i++) {
        if (password[i] == x) {
            password[i] = y;
        } else if (password[i] == y) {
            password[i] = x;
        }
    }
}

void rotateLeft(char *password, int steps, int len) {
    char temp[100]; // Assuming password won't exceed 100 characters
    strcpy(temp, password + steps);
    strncat(temp, password, steps);
    strcpy(password, temp);
}

void rotateRight(char *password, int steps, int len) {
    char temp[100]; // Assuming password won't exceed 100 characters
    strcpy(temp, password + len - steps);
    strncat(temp, password, len - steps);
    strcpy(password, temp);
}

void rotateBasedOnPosition(char *password, char x) {
    int index = strchr(password, x) - password;
    int steps = 1 + index + (index >= 4 ? 1 : 0);
    rotateRight(password, steps % strlen(password), strlen(password));
}

void reversePositions(char *password, int x, int y) {
    while (x < y) {
        swapPosition(password, x, y);
        x++;
        y--;
    }
}

void movePosition(char *password, int x, int y) {
    char temp = password[x];
    memmove(password + x, password + x + 1, strlen(password) - x);
    memmove(password + y + 1, password + y, strlen(password) - y);
    password[y] = temp;
}

void applyOperation(char *operation, char *password) {
    char op[20], arg1[20], arg2[20];
    int x, y;

    if (sscanf(operation, "swap position %d with position %d", &x, &y) == 2) {
        swapPosition(password, x, y);
    } else if (sscanf(operation, "swap letter %s with letter %s", arg1, arg2) == 2) {
        swapLetter(password, arg1[0], arg2[0]);
    } else if (sscanf(operation, "rotate left %d", &x) == 1) {
        rotateLeft(password, x, strlen(password));
    } else if (sscanf(operation, "rotate right %d", &x) == 1) {
        rotateRight(password, x, strlen(password));
    } else if (sscanf(operation, "rotate based on position of letter %s", arg1) == 1) {
        rotateBasedOnPosition(password, arg1[0]);
    } else if (sscanf(operation, "reverse positions %d through %d", &x, &y) == 2) {
        reversePositions(password, x, y);
    } else if (sscanf(operation, "move position %d to position %d", &x, &y) == 2) {
        movePosition(password, x, y);
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        printf("Error reading input file\n");
        return 1;
    }

    char password[] = "abcdefgh";
    char operation[100];

    while (fgets(operation, sizeof(operation), file)) {
        operation[strcspn(operation, "\n")] = 0; // Remove newline character
        applyOperation(operation, password);
    }

    fclose(file);

    printf("%s\n", password);
    return 0;
}
