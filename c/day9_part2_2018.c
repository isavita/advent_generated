
#include <stdio.h>
#include <stdlib.h>

typedef struct Marble {
    int value;
    struct Marble* prev;
    struct Marble* next;
} Marble;

int main() {
    int players, lastMarble;
    FILE *fp;
    char line[256];

    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }
    fgets(line, sizeof(line), fp);
    fclose(fp);

    sscanf(line, "%d players; last marble is worth %d points", &players, &lastMarble);
    lastMarble *= 100;

    long long *scores = (long long*)calloc(players, sizeof(long long));
    if (scores == NULL) {
         perror("Memory allocation failed");
         return 1;
    }

    Marble* current = (Marble*)malloc(sizeof(Marble));
    if (current == NULL){
        perror("Memory allocation failed");
        free(scores);
        return 1;
    }
    current->value = 0;
    current->next = current;
    current->prev = current;

    for (int marble = 1; marble <= lastMarble; marble++) {
        if (marble % 23 == 0) {
            int player = marble % players;
            for (int i = 0; i < 7; i++) {
                current = current->prev;
            }
            scores[player] += (long long)marble + current->value;
            Marble* toRemove = current;
            current->prev->next = current->next;
            current->next->prev = current->prev;
            current = current->next;
            free(toRemove);
        } else {
             current = current->next;
            Marble* newMarble = (Marble*)malloc(sizeof(Marble));
             if (newMarble == NULL){
                  perror("Memory allocation failed");
                  free(scores);

                 Marble* tmp = current;
                 Marble* head = current->next;
                while(head != current){
                   tmp = head;
                   head = head->next;
                    free(tmp);
                }
                  free(current);
                 return 1;
             }
            newMarble->value = marble;
            newMarble->prev = current;
            newMarble->next = current->next;
            current->next->prev = newMarble;
            current->next = newMarble;
            current = newMarble;

        }
    }


    long long maxScore = 0;
    for (int i = 0; i < players; i++) {
        if (scores[i] > maxScore) {
            maxScore = scores[i];
        }
    }

    printf("%lld\n", maxScore);

    free(scores);
    Marble* tmp = current;
    Marble* head = current->next;
    while(head != current){
       tmp = head;
        head = head->next;
        free(tmp);
    }
    free(current);

    return 0;
}
