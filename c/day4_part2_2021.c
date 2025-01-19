
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define BOARD_SIZE 5

typedef struct {
    int board[BOARD_SIZE][BOARD_SIZE];
    bool picked[BOARD_SIZE][BOARD_SIZE];
} BoardState;

BoardState* createBoardState(int board[BOARD_SIZE][BOARD_SIZE]) {
    BoardState* bs = (BoardState*)malloc(sizeof(BoardState));
    if (bs == NULL) {
        perror("Failed to allocate memory for board state");
        exit(EXIT_FAILURE);
    }
    memcpy(bs->board, board, sizeof(bs->board));
    memset(bs->picked, false, sizeof(bs->picked));
    return bs;
}

void freeBoardState(BoardState* bs) {
  free(bs);
}

bool pickNum(BoardState* bs, int num) {
    for (int r = 0; r < BOARD_SIZE; r++) {
        for (int c = 0; c < BOARD_SIZE; c++) {
            if (bs->board[r][c] == num) {
                bs->picked[r][c] = true;
            }
        }
    }

    for (int i = 0; i < BOARD_SIZE; i++) {
        bool isFullRow = true;
        bool isFullCol = true;
        for (int j = 0; j < BOARD_SIZE; j++) {
            if (!bs->picked[i][j]) {
                isFullRow = false;
            }
            if (!bs->picked[j][i]) {
                isFullCol = false;
            }
        }
        if (isFullRow || isFullCol) {
            return true;
        }
    }
    return false;
}

int calculateScore(BoardState* bs) {
    int score = 0;
    for (int r = 0; r < BOARD_SIZE; r++) {
        for (int c = 0; c < BOARD_SIZE; c++) {
            if (!bs->picked[r][c]) {
                score += bs->board[r][c];
            }
        }
    }
    return score;
}

int solve(int* nums, int numCount, BoardState** boards, int boardCount) {
    int lastWinningScore = -1;
    bool* alreadyWon = (bool*)calloc(boardCount, sizeof(bool));
    if(alreadyWon == NULL){
        perror("Failed to allocate memory for already won array");
        exit(EXIT_FAILURE);
    }

    for (int ni = 0; ni < numCount; ni++) {
        for (int bi = 0; bi < boardCount; bi++) {
            if (alreadyWon[bi]) continue;

            if (pickNum(boards[bi], nums[ni])) {
                lastWinningScore = calculateScore(boards[bi]) * nums[ni];
                alreadyWon[bi] = true;
            }
        }
    }
    free(alreadyWon);
    return lastWinningScore;
}


int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Failed to open input file");
        return EXIT_FAILURE;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    // Read the first line (numbers)
    read = getline(&line, &len, fp);
    if (read == -1) {
        perror("Failed to read numbers line");
        fclose(fp);
        return EXIT_FAILURE;
    }

    int nums[100];
    int numCount = 0;
    char *token = strtok(line, ",");
    while (token != NULL) {
        nums[numCount++] = atoi(token);
        token = strtok(NULL, ",");
    }
    free(line);
    line = NULL;
    len = 0;

    // Read the boards
    int boardCount = 0;
    BoardState* boards[100]; 

    while ((read = getline(&line, &len, fp)) != -1) {
        if (strcmp(line, "\n") == 0) continue;
        
        int board[BOARD_SIZE][BOARD_SIZE];
        int row = 0;
        
        char* boardLine = strdup(line);
        char* boardLinePtr = boardLine;
        
        for(;row<BOARD_SIZE;row++){
          
          for(int col=0;col<BOARD_SIZE;col++){
              while(*boardLinePtr==' '){
                boardLinePtr++;
              }
              board[row][col] = atoi(boardLinePtr);
              while(*boardLinePtr>='0' && *boardLinePtr<='9'){
                boardLinePtr++;
              }
              
          }
          if((read = getline(&line, &len, fp)) != -1){
             boardLinePtr = line;
             
          }
          
        }
        free(boardLine);
        
        boards[boardCount++] = createBoardState(board);
        
    }
    
    free(line);
    fclose(fp);


    int result = solve(nums, numCount, boards, boardCount);
    printf("%d\n", result);

    for (int i = 0; i < boardCount; i++) {
        freeBoardState(boards[i]);
    }
    
    return 0;
}
