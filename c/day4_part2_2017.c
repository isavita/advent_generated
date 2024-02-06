
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char* sortString(char* w) {
    int i, j;
    char temp;
    int n = strlen(w);
    for (i = 0; i < n-1; i++) {
        for (j = i+1; j < n; j++) {
            if (w[i] > w[j]) {
                temp = w[i];
                w[i] = w[j];
                w[j] = temp;
            }
        }
    }
    return w;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("File reading error\n");
        return 1;
    }

    char line[1000];
    int validCount = 0;

    while (fgets(line, sizeof(line), file)) {
        char *passphrase = strtok(line, "\n");
        char *word = strtok(passphrase, " ");
        char *wordSet[1000];
        int wordCount = 0;
        int valid = 1;

        while (word != NULL) {
            char sortedWord[1000];
            strcpy(sortedWord, sortString(word));

            int i;
            for (i = 0; i < wordCount; i++) {
                if (strcmp(wordSet[i], sortedWord) == 0) {
                    valid = 0;
                    break;
                }
            }

            if (valid == 0) {
                break;
            }

            wordSet[wordCount] = malloc(strlen(sortedWord) + 1);
            strcpy(wordSet[wordCount], sortedWord);
            wordCount++;

            word = strtok(NULL, " ");
        }

        if (valid == 1) {
            validCount++;
        }
    }

    fclose(file);
    printf("%d\n", validCount);

    return 0;
}
