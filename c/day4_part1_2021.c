
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define BOARD_SIZE 5

typedef struct {
    int numbers[BOARD_SIZE][BOARD_SIZE];
    bool marked[BOARD_SIZE][BOARD_SIZE];
} BingoBoard;

void mark(BingoBoard *board, int number) {
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            if (board->numbers[i][j] == number) {
                board->marked[i][j] = true;
            }
        }
    }
}

bool isRowMarked(bool row[BOARD_SIZE]) {
    for (int i = 0; i < BOARD_SIZE; i++) {
        if (!row[i]) {
            return false;
        }
    }
    return true;
}

bool isColumnMarked(bool marked[BOARD_SIZE][BOARD_SIZE], int column) {
    for (int i = 0; i < BOARD_SIZE; i++) {
        if (!marked[i][column]) {
            return false;
        }
    }
    return true;
}

bool hasWon(BingoBoard *board) {
    for (int i = 0; i < BOARD_SIZE; i++) {
        if (isRowMarked(board->marked[i]) || isColumnMarked(board->marked, i)) {
            return true;
        }
    }
    return false;
}

int unmarkedSum(BingoBoard *board) {
    int sum = 0;
    for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
            if (!board->marked[i][j]) {
                sum += board->numbers[i][j];
            }
        }
    }
    return sum;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    read = getline(&line, &len, file);
    if (read == -1) {
       fclose(file);
       return 1;
    }
    
    int numbers[200];
    int num_count = 0;
    char *token = strtok(line, ",");
    while (token != NULL) {
        numbers[num_count++] = atoi(token);
        token = strtok(NULL, ",");
    }
    free(line);

    BingoBoard boards[100];
    int board_count = 0;

    while (1) {
        
        read = getline(&line, &len, file);
        if (read == -1) {
           break;
        }

        if (line[0] == '\n' || line[0] == '\r') continue;


        BingoBoard *board = &boards[board_count++];
        memset(board->marked, false, sizeof(board->marked));

        
        
        for (int i = 0; i < BOARD_SIZE; i++) {
            
             if (line != NULL && line[0] != '\n') {
                
                int j = 0;
                token = strtok(line, " ");
                while (token != NULL){
                   if (strlen(token) > 0) {
                     board->numbers[i][j++] = atoi(token);
                   }
                   token = strtok(NULL, " ");
                }
             }
             if (i < 4) {
                 getline(&line,&len,file);
              }
        }
        
    }
    free(line);
    fclose(file);


    BingoBoard *winningBoard = NULL;
    int winningNumber = 0;

    for (int i = 0; i < num_count; i++) {
        int number = numbers[i];
        for (int j = 0; j < board_count; j++) {
            BingoBoard *board = &boards[j];
            mark(board, number);
            if (hasWon(board)) {
                winningBoard = board;
                winningNumber = number;
                goto found;
            }
        }
    }
    
found:
    if (winningBoard) {
        printf("%d\n", unmarkedSum(winningBoard) * winningNumber);
    }
    
    return 0;
}
