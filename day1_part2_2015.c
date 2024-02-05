#include <stdio.h>

int main() {
    FILE *fp;
    char ch;
    int floor = 0;
    int position = 0;
    int basement_position = 0;

    fp = fopen("input.txt", "r");

    if (fp == NULL) {
        printf("Error opening file.\n");
        return 1;
    }

    while ((ch = fgetc(fp)) != EOF) {
        position++;
        if (ch == '(') {
            floor++;
        } else if (ch == ')') {
            floor--;
            if (floor == -1 && basement_position == 0) {
                basement_position = position;
            }
        }
    }

    fclose(fp);

    printf("%d\n", floor);
    printf("%d\n", basement_position);

    return 0;
}