
#include <stdio.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    char line[100];
    int totalScore = 0;

    while (fgets(line, sizeof(line), file)) {
        char opponent = line[0];
        char yourMove = line[2];

        int score = 0;
        if (yourMove == 'X') {
            score = 1;
        } else if (yourMove == 'Y') {
            score = 2;
        } else if (yourMove == 'Z') {
            score = 3;
        }

        if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')) {
            score += 6;
        } else if (opponent == 'A' && yourMove == 'X' || opponent == 'B' && yourMove == 'Y' || opponent == 'C' && yourMove == 'Z') {
            score += 3;
        }

        totalScore += score;
    }

    fclose(file);
    printf("%d\n", totalScore);

    return 0;
}
