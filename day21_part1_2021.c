
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error reading file\n");
        exit(1);
    }

    char line1[50], line2[50];
    fgets(line1, sizeof(line1), file);
    fgets(line2, sizeof(line2), file);
    
    int player1Start = atoi(&line1[28]);
    int player2Start = atoi(&line2[28]);
    int player1Pos = player1Start;
    int player2Pos = player2Start;
    int player1Score = 0;
    int player2Score = 0;
    int dieRoll = 1;
    int rollCount = 0;

    while (1) {
        // Player 1
        int rolls = dieRoll%100 + (dieRoll+1)%100 + (dieRoll+2)%100;
        rollCount += 3;
        dieRoll += 3;
        
        player1Pos = (player1Pos+rolls-1)%10 + 1;
        player1Score += player1Pos;

        if (player1Score >= 1000) {
            printf("Result: %d\n", player2Score*rollCount);
            break;
        }

        // Player 2
        rolls = dieRoll%100 + (dieRoll+1)%100 + (dieRoll+2)%100;
        rollCount += 3;
        dieRoll += 3;

        player2Pos = (player2Pos+rolls-1)%10 + 1;
        player2Score += player2Pos;

        if (player2Score >= 1000) {
            printf("%d\n", player1Score * rollCount);
            break;
        }
    }

    fclose(file);
    return 0;
}
