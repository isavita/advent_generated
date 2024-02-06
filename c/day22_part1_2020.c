
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");

    int player1[100];
    int player2[100];
    int p1_size = 0;
    int p2_size = 0;

    int player = 1;
    char line[100];

    while (fgets(line, sizeof(line), fp)) {
        if (line[0] == 'P') {
            continue;
        } else if (line[0] == '\n') {
            player = 2;
            continue;
        } else {
            if (player == 1) {
                player1[p1_size++] = atoi(line);
            } else {
                player2[p2_size++] = atoi(line);
            }
        }
    }

    while (p1_size > 0 && p2_size > 0) {
        int card1 = player1[0];
        int card2 = player2[0];

        for (int i = 0; i < p1_size - 1; i++) {
            player1[i] = player1[i + 1];
        }
        p1_size--;

        for (int i = 0; i < p2_size - 1; i++) {
            player2[i] = player2[i + 1];
        }
        p2_size--;

        if (card1 > card2) {
            player1[p1_size++] = card1;
            player1[p1_size++] = card2;
        } else {
            player2[p2_size++] = card2;
            player2[p2_size++] = card1;
        }
    }

    int score = 0;
    int size = p1_size > 0 ? p1_size : p2_size;
    int *winner = p1_size > 0 ? player1 : player2;

    for (int i = 0; i < size; i++) {
        score += winner[i] * (size - i);
    }

    printf("%d", score);

    fclose(fp);

    return 0;
}
