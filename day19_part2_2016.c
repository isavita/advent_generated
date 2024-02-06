
#include <stdio.h>
#include <stdlib.h>

typedef struct LLNode {
    int elfNum;
    int presents;
    struct LLNode* next;
} LLNode;

int elephant(char* input) {
    int startingElves = atoi(input);
    LLNode* root = (LLNode*)malloc(sizeof(LLNode));
    root->elfNum = 1;
    root->presents = 1;
    LLNode* iter = root;
    for (int i = 2; i <= startingElves; i++) {
        iter->next = (LLNode*)malloc(sizeof(LLNode));
        iter->next->elfNum = i;
        iter->next->presents = 1;
        iter = iter->next;
    }
    iter->next = root;

    int isOddLength = startingElves % 2 == 1;
    LLNode* beforeAcross = root;
    for (int i = 0; i < startingElves / 2 - 1; i++) {
        beforeAcross = beforeAcross->next;
    }

    while (root->next != root) {
        root->presents += beforeAcross->next->presents;

        beforeAcross->next = beforeAcross->next->next;

        if (isOddLength) {
            beforeAcross = beforeAcross->next;
        }
        isOddLength = !isOddLength;
        root = root->next;
    }

    return root->elfNum;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char input[100];
    if (fgets(input, 100, file) != NULL) {
        int ans = elephant(input);
        printf("%d\n", ans);
    } else {
        perror("Error reading file");
        return 1;
    }

    fclose(file);
    return 0;
}
