
#include <stdio.h>
#include <string.h>

int isNiceStringPartOne(char *str) {
    int vowels = 0;
    int doubleLetter = 0;
    int i;
    
    for (i = 0; i < strlen(str); i++) {
        if (str[i] == 'a' || str[i] == 'e' || str[i] == 'i' || str[i] == 'o' || str[i] == 'u') {
            vowels++;
        }
        if (i > 0 && str[i] == str[i-1]) {
            doubleLetter = 1;
        }
        if (strstr(str, "ab") != NULL || strstr(str, "cd") != NULL || strstr(str, "pq") != NULL || strstr(str, "xy") != NULL) {
            return 0;
        }
    }
    
    if (vowels >= 3 && doubleLetter == 1) {
        return 1;
    } else {
        return 0;
    }
}

int isNiceStringPartTwo(char *str) {
    int i, j;
    int pair = 0;
    int repeat = 0;
    
    for (i = 0; i < strlen(str)-1; i++) {
        for (j = i+2; j < strlen(str)-1; j++) {
            if (str[i] == str[j] && str[i+1] == str[j+1]) {
                pair = 1;
            }
        }
    }
    
    for (i = 0; i < strlen(str)-2; i++) {
        if (str[i] == str[i+2]) {
            repeat = 1;
        }
    }
    
    if (pair == 1 && repeat == 1) {
        return 1;
    } else {
        return 0;
    }
}

int main() {
    FILE *fp;
    char str[100];
    int niceCountPartOne = 0;
    int niceCountPartTwo = 0;
    
    fp = fopen("input.txt", "r");
    
    while (fscanf(fp, "%s", str) != EOF) {
        if (isNiceStringPartOne(str)) {
            niceCountPartOne++;
        }
        if (isNiceStringPartTwo(str)) {
            niceCountPartTwo++;
        }
    }
    
    printf("%d\n", niceCountPartOne);
    printf("%d\n", niceCountPartTwo);
    
    fclose(fp);
    
    return 0;
}
