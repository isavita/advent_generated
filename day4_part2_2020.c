
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LENGTH 256
#define MAX_PASSPORT_SIZE 1024

int validateYear(char *value, int min, int max);
int validateHgt(char *value);
int validateHcl(char *value);
int validateEcl(char *value);
int validatePid(char *value);
int isValidPassport(char *passport);

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        printf("Error opening file\n");
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    char passport[MAX_PASSPORT_SIZE] = {0};
    int validPassports = 0;

    while (fgets(line, MAX_LINE_LENGTH, file) != NULL) {
        if (strcmp(line, "\n") == 0 || feof(file)) {
            if (isValidPassport(passport)) {
                validPassports++;
            }
            memset(passport, 0, sizeof(passport));
        } else {
            if (strlen(passport) + strlen(line) < MAX_PASSPORT_SIZE) {
                strncat(passport, line, strlen(line) - 1); // Remove newline
                strcat(passport, " "); // Maintain space separation
            }
        }
    }
    if (passport[0] != '\0' && isValidPassport(passport)) {
        validPassports++;
    }

    printf("%d\n", validPassports);

    fclose(file);
    return 0;
}

int isValidPassport(char *passport) {
    char *fields[] = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"};
    int requiredFields = 7;
    int foundFields = 0;

    for (int i = 0; i < requiredFields; i++) {
        char *field = fields[i];
        char *found = strstr(passport, field);
        if (found) {
            char *value = found + 4; // Move past "xxx:"
            char tempValue[20] = {0};
            sscanf(value, "%s", tempValue);

            int valid = 0;
            if (strcmp(field, "byr") == 0) valid = validateYear(tempValue, 1920, 2002);
            else if (strcmp(field, "iyr") == 0) valid = validateYear(tempValue, 2010, 2020);
            else if (strcmp(field, "eyr") == 0) valid = validateYear(tempValue, 2020, 2030);
            else if (strcmp(field, "hgt") == 0) valid = validateHgt(tempValue);
            else if (strcmp(field, "hcl") == 0) valid = validateHcl(tempValue);
            else if (strcmp(field, "ecl") == 0) valid = validateEcl(tempValue);
            else if (strcmp(field, "pid") == 0) valid = validatePid(tempValue);

            if (valid) foundFields++;
        }
    }

    return foundFields == requiredFields;
}

int validateYear(char *value, int min, int max) {
    int year = atoi(value);
    return year >= min && year <= max;
}

int validateHgt(char *value) {
    int hgt = atoi(value);
    char *unit = value + strlen(value) - 2;
    if (strcmp(unit, "cm") == 0) return hgt >= 150 && hgt <= 193;
    if (strcmp(unit, "in") == 0) return hgt >= 59 && hgt <= 76;
    return 0;
}

int validateHcl(char *value) {
    if (value[0] != '#' || strlen(value) != 7) return 0;
    for (int i = 1; i < 7; i++) {
        if (!isxdigit(value[i])) return 0;
    }
    return 1;
}

int validateEcl(char *value) {
    char *validEcl[] = {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"};
    for (int i = 0; i < 7; i++) {
        if (strcmp(value, validEcl[i]) == 0) return 1;
    }
    return 0;
}

int validatePid(char *value) {
    if (strlen(value) != 9) return 0;
    for (int i = 0; i < 9; i++) {
        if (!isdigit(value[i])) return 0;
    }
    return 1;
}
