
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef struct {
    int positions[2];
    int scores[2];
    int rollsLeftInTurn;
    int isPlayer1sTurn;
} GameState;

typedef struct {
    int64_t wins1;
    int64_t wins2;
} WinCounts;

typedef struct {
    GameState state;
    WinCounts counts;
    struct Node* next;
} Node;


Node *head = NULL;

WinCounts play(GameState state, Node **cache) {
    
    for (Node *current = *cache; current != NULL; current = current->next) {
            if (memcmp(&current->state, &state, sizeof(GameState)) == 0) {
                return current->counts;
            }
        }
    
    WinCounts wins = {0, 0};
    int playerIndex = state.isPlayer1sTurn ? 0 : 1;
    
    if (state.rollsLeftInTurn == 0) {
        state.scores[playerIndex] += state.positions[playerIndex];
        if(state.scores[playerIndex] >= 21){
            wins.wins1 = (playerIndex == 0);
            wins.wins2 = (playerIndex == 1);
             Node *newNode = malloc(sizeof(Node));
            if (newNode == NULL) {
                perror("Memory allocation failed");
                exit(EXIT_FAILURE);
            }
            newNode->state = state;
            newNode->counts = wins;
            newNode->next = *cache;
            *cache = newNode;

            return wins;
        }
        state.isPlayer1sTurn = !state.isPlayer1sTurn;
        state.rollsLeftInTurn = 3;
        playerIndex = (playerIndex + 1) % 2;
    }

    for (int roll = 1; roll <= 3; roll++) {
        GameState nextState = state;
        nextState.positions[playerIndex] += roll;
        if (nextState.positions[playerIndex] > 10) {
            nextState.positions[playerIndex] -= 10;
        }
        nextState.rollsLeftInTurn--;
        WinCounts nextWins = play(nextState, cache);
        wins.wins1 += nextWins.wins1;
        wins.wins2 += nextWins.wins2;
    }
    Node *newNode = malloc(sizeof(Node));
    if (newNode == NULL) {
        perror("Memory allocation failed");
        exit(EXIT_FAILURE);
    }
    newNode->state = state;
    newNode->counts = wins;
    newNode->next = *cache;
    *cache = newNode;
    return wins;
}


int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    int positions[2];
    fscanf(file, "Player 1 starting position: %d\n", &positions[0]);
    fscanf(file, "Player 2 starting position: %d", &positions[1]);
    fclose(file);

    GameState initialState;
    initialState.positions[0] = positions[0];
    initialState.positions[1] = positions[1];
    initialState.scores[0] = 0;
    initialState.scores[1] = 0;
    initialState.rollsLeftInTurn = 3;
    initialState.isPlayer1sTurn = 1;
     Node* cache = NULL;
    WinCounts finalWins = play(initialState,&cache);
    
    if (finalWins.wins1 > finalWins.wins2) {
          printf("%lld\n", finalWins.wins1);
    } else {
          printf("%lld\n", finalWins.wins2);
    }
    
    while (cache != NULL) {
        Node *temp = cache;
        cache = cache->next;
        free(temp);
    }
    return 0;
}
