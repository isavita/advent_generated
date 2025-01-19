
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

typedef struct SnailNumber {
    int value;
    struct SnailNumber *left;
    struct SnailNumber *right;
} SnailNumber;

bool isRegular(SnailNumber *sn) {
    return sn->left == NULL && sn->right == NULL;
}

SnailNumber* createRegularSnailNumber(int value) {
    SnailNumber* sn = (SnailNumber*)malloc(sizeof(SnailNumber));
    sn->value = value;
    sn->left = NULL;
    sn->right = NULL;
    return sn;
}

SnailNumber* createPairSnailNumber(SnailNumber* left, SnailNumber* right) {
    SnailNumber* sn = (SnailNumber*)malloc(sizeof(SnailNumber));
    sn->value = -1;
    sn->left = left;
    sn->right = right;
    return sn;
}

SnailNumber* add(SnailNumber *sn1, SnailNumber *sn2);
SnailNumber* reduce(SnailNumber *sn);

bool explode(SnailNumber *sn, int depth, int* leftValue, int* rightValue);
void addLeft(SnailNumber *sn, int value);
void addRight(SnailNumber *sn, int value);
bool split(SnailNumber *sn);
int magnitude(SnailNumber *sn);
SnailNumber* parseSnailNumber(const char *input);

SnailNumber* deepCopy(SnailNumber* sn);
void freeSnailNumber(SnailNumber* sn);

SnailNumber* add(SnailNumber *sn1, SnailNumber *sn2) {
    SnailNumber *newNumber = createPairSnailNumber(sn1, sn2);
    return reduce(newNumber);
}

SnailNumber* reduce(SnailNumber *sn) {
    int leftValue, rightValue;
    while (true) {
        if (explode(sn, 0, &leftValue, &rightValue)) {
            continue;
        }
        if (!split(sn)) {
            break;
        }
    }
    return sn;
}

bool explode(SnailNumber *sn, int depth, int* leftValue, int* rightValue) {
    if (isRegular(sn)) {
        return false;
    }

    if (depth == 4) {
        *leftValue = sn->left->value;
        *rightValue = sn->right->value;
        free(sn->left);
        free(sn->right);
        sn->left = NULL;
        sn->right = NULL;
        sn->value = 0;
        return true;
    }

    if (explode(sn->left, depth + 1, leftValue, rightValue)) {
        if (*rightValue > 0 && sn->right != NULL) {
           addLeft(sn->right, *rightValue);
        }
        *rightValue = 0;
        return true;
    }

    if (explode(sn->right, depth + 1, leftValue, rightValue)) {
        if (*leftValue > 0 && sn->left != NULL) {
            addRight(sn->left, *leftValue);
        }
        *leftValue = 0;
        return true;
    }

    return false;
}


void addLeft(SnailNumber *sn, int value) {
    if (isRegular(sn)) {
        sn->value += value;
    } else {
        addLeft(sn->left, value);
    }
}

void addRight(SnailNumber *sn, int value) {
    if (isRegular(sn)) {
        sn->value += value;
    } else {
        addRight(sn->right, value);
    }
}


bool split(SnailNumber *sn) {
    if (isRegular(sn)) {
        if (sn->value >= 10) {
            sn->left = createRegularSnailNumber(sn->value / 2);
            sn->right = createRegularSnailNumber((sn->value + 1) / 2);
            sn->value = -1;
            return true;
        }
        return false;
    }
    return split(sn->left) || split(sn->right);
}

int magnitude(SnailNumber *sn) {
    if (isRegular(sn)) {
        return sn->value;
    }
    return 3 * magnitude(sn->left) + 2 * magnitude(sn->right);
}

SnailNumber* parseSnailNumber(const char *input) {
    input = strdup(input);
    char* trimmedInput = input;
    while (isspace(*trimmedInput)){
        trimmedInput++;
    }

    if (*trimmedInput != '[') {
        int value = atoi(trimmedInput);
        free(input);
        return createRegularSnailNumber(value);
    }
    int balance = 0;
    int splitIndex = 0;
    int len = strlen(trimmedInput);
    for (int i = 1; i < len - 1; i++) {
        switch (trimmedInput[i]) {
            case '[':
                balance++;
                break;
            case ']':
                balance--;
                break;
            case ',':
                if (balance == 0) {
                    splitIndex = i;
                    break;
                }
        }
        if (splitIndex != 0) {
            break;
        }
    }

    char* leftStr = (char*)malloc(splitIndex + 1);
    strncpy(leftStr, trimmedInput + 1, splitIndex);
    leftStr[splitIndex] = '\0';
    
    char* rightStr = (char*)malloc(len - splitIndex);
    strncpy(rightStr, trimmedInput + splitIndex + 1, len - splitIndex - 2);
    rightStr[len - splitIndex - 2] = '\0';

    SnailNumber *left = parseSnailNumber(leftStr);
    SnailNumber *right = parseSnailNumber(rightStr);

    free(leftStr);
    free(rightStr);
    free(input);

    return createPairSnailNumber(left, right);
}


SnailNumber* deepCopy(SnailNumber* sn) {
    if (isRegular(sn)) {
        return createRegularSnailNumber(sn->value);
    }
    return createPairSnailNumber(deepCopy(sn->left), deepCopy(sn->right));
}

void freeSnailNumber(SnailNumber* sn) {
    if (sn == NULL) {
        return;
    }
    freeSnailNumber(sn->left);
    freeSnailNumber(sn->right);
    free(sn);
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening input file");
        return 1;
    }
    
    char line[256];
    SnailNumber* snailNumbers[200];
    int snailCount = 0;
    
    while (fgets(line, sizeof(line), file) != NULL) {
        snailNumbers[snailCount++] = parseSnailNumber(line);
    }
    
    fclose(file);
    
    if(snailCount == 0)
    {
        printf("No snailfish numbers found in the file.\n");
        return 1;
    }

    int largestMagnitude = 0;
    for(int i = 0; i < snailCount; i++)
    {
        for(int j = 0; j < snailCount; j++)
        {
            if(i == j) continue;

            SnailNumber* aCopy = deepCopy(snailNumbers[i]);
            SnailNumber* bCopy = deepCopy(snailNumbers[j]);

            int sum1 = magnitude(add(aCopy, deepCopy(snailNumbers[j])));
            int sum2 = magnitude(add(bCopy, deepCopy(snailNumbers[i])));
            
            if (sum1 > largestMagnitude) {
                largestMagnitude = sum1;
            }
            if (sum2 > largestMagnitude) {
                largestMagnitude = sum2;
            }
            freeSnailNumber(aCopy);
            freeSnailNumber(bCopy);
        }
    }
    
    for(int i = 0; i < snailCount; i++) {
        freeSnailNumber(snailNumbers[i]);
    }

    printf("%d\n", largestMagnitude);
    
    return 0;
}
