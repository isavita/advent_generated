
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int check_passphrase(char** words, int num_words) {
    for (int i = 0; i < num_words; i++) {
        for (int j = i + 1; j < num_words; j++) {
            if (strcmp(words[i], words[j]) == 0) {
                return 0;
            }
        }
    }
    return 1;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        return 1;
    }

    int valid_passphrases = 0;
    char line[1000];

    while (fgets(line, sizeof(line), fp)) {
        char* word = strtok(line, " \n");
        char* words[100];
        int num_words = 0;

        while (word != NULL) {
            words[num_words++] = strdup(word);
            word = strtok(NULL, " \n");
        }

        if (check_passphrase(words, num_words)) {
            valid_passphrases++;
        }

        for (int i = 0; i < num_words; i++) {
            free(words[i]);
        }
    }

    fclose(fp);

    printf("%d\n", valid_passphrases);

    return 0;
}
