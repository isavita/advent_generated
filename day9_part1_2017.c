#include <stdio.h>

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        return 1;
    }

    int score = 0;
    int depth = 0;
    char c;
    int in_garbage = 0;
    int ignore_next = 0;

    while ((c = fgetc(fp)) != EOF) {
        if (ignore_next) {
            ignore_next = 0;
            continue;
        }

        if (in_garbage) {
            if (c == '!') {
                ignore_next = 1;
            } else if (c == '>') {
                in_garbage = 0;
            }
        } else {
            if (c == '{') {
                depth++;
            } else if (c == '}') {
                score += depth;
                depth--;
            } else if (c == '<') {
                in_garbage = 1;
            }
        }
    }

    fclose(fp);

    printf("%d\n", score);

    return 0;
}