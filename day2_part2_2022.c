
#include <stdio.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char opponent, roundEnd;
    int totalScore = 0;

    while (fscanf(file, "%c %c\n", &opponent, &roundEnd) == 2) {
        char yourMove = ' ';
        if (roundEnd == 'X') {
            if (opponent == 'A') yourMove = 'Z';
            else if (opponent == 'B') yourMove = 'X';
            else yourMove = 'Y';
        } else if (roundEnd == 'Y') {
            if (opponent == 'A') yourMove = 'X';
            else if (opponent == 'B') yourMove = 'Y';
            else yourMove = 'Z';
        } else {
            if (opponent == 'A') yourMove = 'Y';
            else if (opponent == 'B') yourMove = 'Z';
            else yourMove = 'X';
        }

        int score = 0;
        if (yourMove == 'X') score = 1;
        else if (yourMove == 'Y') score = 2;
        else if (yourMove == 'Z') score = 3;

        if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')) {
            score += 6;
        } else if ((opponent == 'A' && yourMove == 'X') || (opponent == 'B' && yourMove == 'Y') || (opponent == 'C' && yourMove == 'Z')) {
            score += 3;
        }

        totalScore += score;
    }

    if (ferror(file)) {
        printf("Error scanning file\n");
        fclose(file);
        return 1;
    }

    fclose(file);
    printf("%d\n", totalScore);

    return 0;
}
