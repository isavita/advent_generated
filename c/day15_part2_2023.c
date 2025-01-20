
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HASH_TABLE_SIZE 256

typedef struct {
    char label[10];
    int numBox;
    char operation;
    int number;
} Step;

int hashString(const char *str) {
    int res = 0;
    for (int i = 0; str[i] != '\0'; i++) {
        res += str[i];
        res *= 17;
        res %= HASH_TABLE_SIZE;
    }
    return res;
}

Step parseStep(const char *stepStr) {
    Step step;
    int i = 0;

    while (stepStr[i] != '\0' && stepStr[i] != '=' && stepStr[i] != '-') {
        step.label[i] = stepStr[i];
        i++;
    }
    step.label[i] = '\0';

    step.numBox = hashString(step.label);
    step.operation = stepStr[i];

    if (step.operation == '=') {
        step.number = atoi(stepStr + i + 1);
    }

    return step;
}

typedef struct {
    char label[10];
    int focalLength;
} Lens;

typedef struct Box {
    Lens lenses[10];
    int count;
} Box;

void processStep(Box boxes[HASH_TABLE_SIZE], const Step *step) {
    Box *box = &boxes[step->numBox];
    int i;

    for (i = 0; i < box->count; i++) {
        if (strcmp(box->lenses[i].label, step->label) == 0) {
            break;
        }
    }

    if (step->operation == '-') {
        if (i < box->count) {
            for (int j = i; j < box->count - 1; j++) {
                box->lenses[j] = box->lenses[j + 1];
            }
            box->count--;
        }
    } else if (step->operation == '=') {
        if (i < box->count) {
            box->lenses[i].focalLength = step->number;
        } else {
            strcpy(box->lenses[box->count].label, step->label);
            box->lenses[box->count].focalLength = step->number;
            box->count++;
        }
    }
}

int calculatePower(const Box boxes[HASH_TABLE_SIZE]) {
    int res = 0;

    for (int iBox = 0; iBox < HASH_TABLE_SIZE; iBox++) {
        for (int iSlot = 0; iSlot < boxes[iBox].count; iSlot++) {
            res += (iBox + 1) * (iSlot + 1) * boxes[iBox].lenses[iSlot].focalLength;
        }
    }

    return res;
}

int solve(const char *input) {
    Box boxes[HASH_TABLE_SIZE] = {0};
    char *token = strtok((char *)input, ",");
    while (token != NULL) {
        Step step = parseStep(token);
        processStep(boxes, &step);
        token = strtok(NULL, ",");
    }

    return calculatePower(boxes);
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *input = (char *)malloc(length + 1);
    if (input == NULL) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(input, 1, length, file);
    input[length] = '\0';

    fclose(file);

    int result = solve(input);
    printf("%d\n", result);

    free(input);
    return 0;
}
