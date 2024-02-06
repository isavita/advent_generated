#include <stdio.h>

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        return 1;
    }

    int floor = 0;
    char c;
    while ((c = fgetc(fp)) != EOF) {
        if (c == '(') {
            floor++;
        } else if (c == ')') {
            floor--;
        }
    }

    fclose(fp);

    printf("%d\n", floor);

    return 0;
}