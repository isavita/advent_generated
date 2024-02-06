
#include <stdio.h>

int isNice(char *str) {
    int vowels = 0;
    int doubleLetter = 0;
    int i;

    for (i = 0; str[i] != '\0'; i++) {
        if (str[i] == 'a' || str[i] == 'e' || str[i] == 'i' || str[i] == 'o' || str[i] == 'u') {
            vowels++;
        }
        if (str[i] == str[i + 1]) {
            doubleLetter = 1;
        }
        if (str[i] == 'a' && str[i + 1] == 'b') {
            return 0;
        }
        if (str[i] == 'c' && str[i + 1] == 'd') {
            return 0;
        }
        if (str[i] == 'p' && str[i + 1] == 'q') {
            return 0;
        }
        if (str[i] == 'x' && str[i + 1] == 'y') {
            return 0;
        }
    }

    if (vowels >= 3 && doubleLetter == 1) {
        return 1;
    } else {
        return 0;
    }
}

int main() {
    FILE *fp;
    char str[100];
    int niceStrings = 0;

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        return 1;
    }

    while (fgets(str, 100, fp) != NULL) {
        if (isNice(str)) {
            niceStrings++;
        }
    }

    fclose(fp);

    printf("%d\n", niceStrings);

    return 0;
}
