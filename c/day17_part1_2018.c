
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LENGTH 256
#define MAX_GROUND_SIZE 2000

typedef struct {
    char** data;
    int rows;
    int cols;
} Ground;


int strToInt(const char *str) {
    int res = 0;
    int sign = 1;
    int i = 0;
    if (str[0] == '-') {
        sign = -1;
        i++;
    }
    for (; str[i] != '\0' && isdigit(str[i]); i++) {
        res = res * 10 + (str[i] - '0');
    }
    return res * sign;
}
char** allocateGround(int rows, int cols) {
    char** ground = (char**)malloc(rows * sizeof(char*));
    if(ground == NULL) return NULL;
    for(int i = 0; i < rows; i++) {
        ground[i] = (char*)malloc(cols * sizeof(char));
        if(ground[i] == NULL) {
            for(int j=0; j < i; j++){
              free(ground[j]);
            }
            free(ground);
            return NULL;
        }
    }
    return ground;
}
void freeGround(Ground* ground) {
    if (ground && ground->data) {
        for (int i = 0; i < ground->rows; i++) {
            free(ground->data[i]);
        }
        free(ground->data);
    }
}
Ground createGround(int initialRows, int initialCols){
    Ground ground;
    ground.rows = initialRows;
    ground.cols = initialCols;
    ground.data = allocateGround(ground.rows, ground.cols);
    if(ground.data == NULL){
      fprintf(stderr, "Memory allocation failed\n");
      exit(1);
    }
    for(int i = 0; i < ground.rows; i++){
        for(int j = 0; j < ground.cols; j++){
            ground.data[i][j] = '.';
        }
    }
    ground.data[0][0] = '+';
    return ground;
}
void growGround(Ground* ground, int newRows, int newCols){
    char** newGroundData = allocateGround(newRows, newCols);
     if(newGroundData == NULL){
       fprintf(stderr, "Memory allocation failed\n");
       exit(1);
    }
    for(int i = 0; i < ground->rows; i++){
        for(int j = 0; j < ground->cols; j++){
            newGroundData[i][j] = ground->data[i][j];
        }
         for(int j = ground->cols; j < newCols; j++){
            newGroundData[i][j] = '.';
        }
    }
    for (int i = ground->rows; i < newRows; i++){
         for(int j = 0; j < newCols; j++){
            newGroundData[i][j] = '.';
         }
    }
    freeGround(ground);
    ground->data = newGroundData;
    ground->rows = newRows;
    ground->cols = newCols;

}

void shiftGround(Ground* ground, int shift){

    int newCols = ground->cols + shift;
    char** newGroundData = allocateGround(ground->rows, newCols);
    if(newGroundData == NULL){
       fprintf(stderr, "Memory allocation failed\n");
       exit(1);
    }
    for(int i = 0; i < ground->rows; i++){
        for(int j = 0; j < shift; j++){
            newGroundData[i][j] = '.';
        }
        for(int j = 0; j < ground->cols; j++){
            newGroundData[i][j+shift] = ground->data[i][j];
        }
    }
    freeGround(ground);
    ground->data = newGroundData;
    ground->cols = newCols;

}

void parseLine(Ground *ground, char *line, int *minX, int *maxX, int *minY, int *maxY, int xOffset, int yOffset) {
    char *token;
    char *tokens[6];
    int i = 0;

    token = strtok(line, "=, .");
    while (token != NULL && i < 6) {
        tokens[i++] = token;
        token = strtok(NULL, "=, .");
    }

    if (tokens[0][0] == 'x') {
        int x = strToInt(tokens[1]) - xOffset;
        int y1 = strToInt(tokens[3]) - yOffset;
        int y2 = strToInt(tokens[4]) - yOffset;

        while (x >= *maxX) {
            *maxX +=1;
             growGround(ground, ground->rows, *maxX - *minX + 1);
        }
        while (x <= *minX) {
            *minX -= 1;
            shiftGround(ground, 1);
        }
        while (y2 > *maxY) {
            *maxY += 1;
            growGround(ground, *maxY + 1, ground->cols);
        }
        if (y1 < *minY) {
            *minY = y1;
        }
        for (int i = y1; i <= y2; i++) {
            ground->data[i][x - *minX] = '#';
        }
    } else {
        int y = strToInt(tokens[1]) - yOffset;
        int x1 = strToInt(tokens[3]) - xOffset;
        int x2 = strToInt(tokens[4]) - xOffset;

        while (y > *maxY) {
             *maxY += 1;
             growGround(ground, *maxY + 1, ground->cols);
        }
        while (x2 >= *maxX) {
            *maxX += 1;
             growGround(ground, ground->rows, *maxX - *minX + 1);
        }
        while (x1 <= *minX) {
             *minX -= 1;
             shiftGround(ground, 1);
        }
        for (int i = x1; i <= x2; i++) {
            ground->data[y][i - *minX] = '#';
        }
        if (y < *minY) {
            *minY = y;
        }
    }
}
int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    Ground ground = createGround(1, 1);
    int maxX = 0, minX = 0, maxY = 0, minY = 20;
    int xOffset = 500, yOffset = 0;

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = '\0';
        parseLine(&ground, line, &minX, &maxX, &minY, &maxY, xOffset, yOffset);
    }
    fclose(file);

    int waterCount = 0;
    int flowCount = 0;
    int roundLimit = 200000;
    int x, y, tryLeft;

    while (ground.data[1][-minX] != '|' && waterCount < roundLimit) {
        x = -minX;
        y = 1;
        tryLeft = 0;
        int canMove = 1;
        while(canMove){
          if (y + 1 > maxY || ground.data[y + 1][x] == '|') {
              ground.data[y][x] = '|';
              canMove = 0;
              if (y >= minY) {
                  flowCount++;
              }
          } else if (ground.data[y + 1][x] == '.') {
              y++;
              tryLeft = 0;
          } else if (ground.data[y + 1][x] == '#' || ground.data[y + 1][x] == '~') {
            if ((tryLeft == 1 && ground.data[y][x - 1] == '|') ||
                (tryLeft == 2 && ground.data[y][x + 1] == '|') ||
                (ground.data[y][x + 1] == '|' && ground.data[y][x - 1] != '.') ||
                (ground.data[y][x + 1] != '.' && ground.data[y][x - 1] == '|')) {
                ground.data[y][x] = '|';
                flowCount++;
                canMove = 0;
                for (int i = x + 1; ground.data[y][i] == '~'; i++) {
                  ground.data[y][i] = '|';
                  waterCount--;
                  flowCount++;
                }
                for (int i = x - 1; ground.data[y][i] == '~'; i--) {
                    ground.data[y][i] = '|';
                    waterCount--;
                    flowCount++;
                }
              } else if ((tryLeft == 0 && ground.data[y][x - 1] == '.') ||
                          (tryLeft == 1 && ground.data[y][x - 1] == '.')) {
                  x--;
                  tryLeft = 1;
              } else if ((tryLeft == 0 && ground.data[y][x + 1] == '.') ||
                          (tryLeft == 2 && ground.data[y][x + 1] == '.')) {
                  x++;
                  tryLeft = 2;
              } else {
                  canMove = 0;
                  ground.data[y][x] = '~';
                  waterCount++;
              }
          }
      }
    }
    printf("%d\n", flowCount + waterCount);
    freeGround(&ground);
    return 0;
}
