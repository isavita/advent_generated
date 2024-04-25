#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Marble {
    int value;
    struct Marble* prev;
    struct Marble* next;
} Marble;

int readInput(const char* filename, int* players, int* lastMarble) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        return 1;
    }

    char line[1024];
    fgets(line, 1024, file);
    fclose(file);

    char* parts[7];
    char* token = strtok(line, " ");
    int i = 0;
    while (token != NULL && i < 7) {
        parts[i++] = token;
        token = strtok(NULL, " ");
    }

    *players = atoi(parts[0]);
    *lastMarble = atoi(parts[6]);

    return 0;
}

int playMarbleGame(int players, int lastMarble) {
    int scores[players];
    for (int i = 0; i < players; i++) {
        scores[i] = 0;
    }

    Marble* current = (Marble*)malloc(sizeof(Marble));
    current->value = 0;
    current->prev = current;
    current->next = current;

    for (int marble = 1; marble <= lastMarble; marble++) {
        if (marble % 23 == 0) {
            int player = marble % players;
            for (int i = 0; i < 7; i++) {
                current = current->prev;
            }
            scores[player] += marble + current->value;
            current->prev->next = current->next;
            current->next->prev = current->prev;
            current = current->next;
        } else {
            current = current->next;
            Marble* newMarble = (Marble*)malloc(sizeof(Marble));
            newMarble->value = marble;
            newMarble->prev = current;
            newMarble->next = current->next;
            current->next->prev = newMarble;
            current->next = newMarble;
            current = newMarble;
        }
    }

    int maxScore = 0;
    for (int i = 0; i < players; i++) {
        if (scores[i] > maxScore) {
            maxScore = scores[i];
        }
    }

    return maxScore;
}

int main() {
    int players, lastMarble;
    if (readInput("input.txt", &players, &lastMarble) != 0) {
        return 1;
    }

    printf("%d\n", playMarbleGame(players, lastMarble));

    return 0;
}