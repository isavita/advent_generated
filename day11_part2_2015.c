
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

bool hasStraight(const char *password) {
    for (int i = 0; password[i] && password[i+1] && password[i+2]; i++) {
        if (password[i] + 1 == password[i+1] && password[i] + 2 == password[i+2])
            return true;
    }
    return false;
}

bool containsInvalidLetters(const char *password) {
    for (int i = 0; password[i]; i++) {
        if (password[i] == 'i' || password[i] == 'o' || password[i] == 'l')
            return true;
    }
    return false;
}

bool hasTwoPairs(const char *password) {
    int count = 0;
    for (int i = 0; password[i] && password[i+1]; i++) {
        if (password[i] == password[i+1]) {
            count++;
            i++; // Skip the next character
        }
    }
    return count >= 2;
}

bool isValidPassword(const char *password) {
    return hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password);
}

void incrementPassword(char *password) {
    int len = strlen(password);
    for (int i = len - 1; i >= 0; i--) {
        if (password[i] == 'z') {
            password[i] = 'a';
        } else {
            password[i]++;
            if (password[i] == 'i' || password[i] == 'o' || password[i] == 'l') {
                password[i]++;
            }
            break;
        }
    }
}

void findNextPassword(char *password) {
    do {
        incrementPassword(password);
    } while (!isValidPassword(password));
}

int main() {
    FILE *file;
    char currentPassword[100];

    file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error reading input.\n");
        return 1;
    }
    if (fscanf(file, "%s", currentPassword) != 1) {
        printf("Failed to read password.\n");
        fclose(file);
        return 1;
    }
    fclose(file);

    findNextPassword(currentPassword);
    findNextPassword(currentPassword);

    printf("%s\n", currentPassword);
    return 0;
}
