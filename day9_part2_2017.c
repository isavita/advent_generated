
#include <stdio.h>

int main() {
    FILE *fp = fopen("input.txt", "r");
    int score = 0, garbageCount = 0, depth = 0;
    char c;
    int inGarbage = 0, ignoreNext = 0;

    while((c = fgetc(fp)) != EOF) {
        if (ignoreNext) {
            ignoreNext = 0;
            continue;
        }

        if (inGarbage) {
            if (c == '!') {
                ignoreNext = 1;
            } else if (c == '>') {
                inGarbage = 0;
            } else {
                garbageCount++;
            }
        } else {
            if (c == '{') {
                depth++;
                score += depth;
            } else if (c == '}') {
                depth--;
            } else if (c == '<') {
                inGarbage = 1;
            }
        }
    }

    printf("%d\n", score);
    printf("%d\n", garbageCount);

    fclose(fp);
    return 0;
}
