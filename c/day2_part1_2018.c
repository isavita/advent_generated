
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp;
    char boxID[30];
    int countTwo = 0, countThree = 0;

    fp = fopen("input.txt", "r");

    while (fscanf(fp, "%s", boxID) != EOF) {
        int charCount[26] = {0};
        int hasTwo = 0, hasThree = 0;

        for (int i = 0; i < strlen(boxID); i++) {
            charCount[boxID[i] - 'a']++;
        }

        for (int i = 0; i < 26; i++) {
            if (charCount[i] == 2) {
                hasTwo = 1;
            } else if (charCount[i] == 3) {
                hasThree = 1;
            }
        }

        countTwo += hasTwo;
        countThree += hasThree;
    }

    fclose(fp);

    printf("%d\n", countTwo * countThree);

    return 0;
}
