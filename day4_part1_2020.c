
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int isValid(char *passport, char **requiredFields, int numFields) {
    for (int i = 0; i < numFields; i++) {
        if (strstr(passport, requiredFields[i]) == NULL) {
            return 0;
        }
    }
    return 1;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char line[1000];
    char passport[10000] = "";
    char **passports = malloc(1000 * sizeof(char *));
    int numPassports = 0;

    while (fgets(line, sizeof(line), file)) {
        if (line[0] == '\n') {
            passports[numPassports] = malloc(strlen(passport) + 1);
            strcpy(passports[numPassports], passport);
            numPassports++;
            passport[0] = '\0';
        } else {
            strcat(passport, " ");
            strcat(passport, line);
        }
    }
    if (strlen(passport) > 0) {
        passports[numPassports] = malloc(strlen(passport) + 1);
        strcpy(passports[numPassports], passport);
        numPassports++;
    }

    int validPassports = 0;
    char *requiredFields[] = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"};
    int numFields = sizeof(requiredFields) / sizeof(requiredFields[0]);

    for (int i = 0; i < numPassports; i++) {
        if (isValid(passports[i], requiredFields, numFields)) {
            validPassports++;
        }
    }

    printf("%d\n", validPassports);

    for (int i = 0; i < numPassports; i++) {
        free(passports[i]);
    }
    free(passports);
    fclose(file);

    return 0;
}
