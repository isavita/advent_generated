
#include <stdio.h>

int isValidPassword(char *password) {
    int straight = 0;
    int pairs = 0;
    char prev = '\0';
    char prev2 = '\0';
    int i;

    for (i = 0; password[i] != '\0'; i++) {
        if (password[i] == 'i' || password[i] == 'o' || password[i] == 'l') {
            return 0;
        }

        if (password[i] == prev && password[i] != prev2) {
            pairs++;
            prev = '\0';
        } else if (password[i] == prev) {
            prev = '\0';
        } else {
            prev2 = prev;
            prev = password[i];
        }

        if (i >= 2 && password[i] == password[i - 1] + 1 && password[i - 1] == password[i - 2] + 1) {
            straight = 1;
        }
    }

    return straight && pairs >= 2;
}

void incrementPassword(char *password) {
    int i = 7;
    while (i >= 0) {
        if (password[i] == 'z') {
            password[i] = 'a';
            i--;
        } else {
            password[i]++;
            break;
        }
    }
}

int main() {
    FILE *file = fopen("input.txt", "r");
    char password[9];
    fscanf(file, "%s", password);
    fclose(file);

    do {
        incrementPassword(password);
    } while (!isValidPassword(password));

    printf("%s", password);

    return 0;
}
