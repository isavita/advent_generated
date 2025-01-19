
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

bool checkSequence(int *scoreboard, int scoreboardLen, int *sequence, int sequenceLen) {
    if (scoreboardLen < sequenceLen) return false;
    int start = scoreboardLen - sequenceLen;
    for (int i = 0; i < sequenceLen; i++) {
        if (scoreboard[start + i] != sequence[i]) return false;
    }
    return true;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char input[20];
    if (fscanf(file, "%s", input) != 1) {
        fclose(file);
        return 1;
    }
    fclose(file);

    int inputLen = strlen(input);
    int *inputSequence = malloc(inputLen * sizeof(int));
    if (!inputSequence) return 1;
    for (int i = 0; i < inputLen; i++) {
        inputSequence[i] = input[i] - '0';
    }

    int scoreboardCapacity = 10000000;
    int *scoreboard = malloc(scoreboardCapacity * sizeof(int));
    if (!scoreboard) {
        free(inputSequence);
        return 1;
    }
    scoreboard[0] = 3;
    scoreboard[1] = 7;
    int scoreboardLen = 2;
    int elf1 = 0, elf2 = 1;

    while (true) {
        int newScore = scoreboard[elf1] + scoreboard[elf2];
        if (newScore >= 10) {
           if(scoreboardLen == scoreboardCapacity){
                scoreboardCapacity *=2;
                int* tmp = realloc(scoreboard, scoreboardCapacity * sizeof(int));
                if(!tmp){
                    free(scoreboard);
                    free(inputSequence);
                    return 1;
                }
                scoreboard = tmp;
            }
            scoreboard[scoreboardLen++] = newScore / 10;
            if (checkSequence(scoreboard, scoreboardLen, inputSequence, inputLen)) break;
        }
        if(scoreboardLen == scoreboardCapacity){
            scoreboardCapacity *=2;
             int* tmp = realloc(scoreboard, scoreboardCapacity * sizeof(int));
                if(!tmp){
                    free(scoreboard);
                    free(inputSequence);
                    return 1;
                }
                scoreboard = tmp;
        }
        scoreboard[scoreboardLen++] = newScore % 10;
        if (checkSequence(scoreboard, scoreboardLen, inputSequence, inputLen)) break;

        elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboardLen;
        elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboardLen;
    }

    printf("%d\n", scoreboardLen - inputLen);
    free(scoreboard);
    free(inputSequence);
    return 0;
}
