
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LENGTH 256
#define MAX_GROUND_SIZE 2000

typedef struct {
    char **data;
    int rows;
    int cols;
} Ground;

Ground createGround() {
    Ground g;
    g.rows = 1;
    g.cols = 1;
    g.data = (char **)malloc(sizeof(char *) * g.rows);
    g.data[0] = (char *)malloc(sizeof(char) * g.cols);
    g.data[0][0] = '+';
    return g;
}

void freeGround(Ground *g) {
    if (!g) return;
    for (int i = 0; i < g->rows; i++) {
        free(g->data[i]);
    }
    free(g->data);
    g->data = NULL;
    g->rows = 0;
    g->cols = 0;
}

void expandGround(Ground *g, int newRows, int newCols) {
    if (!g) return;
    if (newRows > g->rows) {
        g->data = (char **)realloc(g->data, sizeof(char *) * newRows);
        for(int i = g->rows; i < newRows; i++) {
          g->data[i] = (char *)malloc(sizeof(char) * g->cols);
          for (int j = 0; j < g->cols; j++)
            g->data[i][j] = '.';
        }
        g->rows = newRows;
    }
    if (newCols > g->cols) {
        for(int i = 0; i < g->rows; i++) {
          g->data[i] = (char *)realloc(g->data[i], sizeof(char) * newCols);
          for (int j = g->cols; j < newCols; j++)
            g->data[i][j] = '.';
        }
        g->cols = newCols;
    }
}
void shiftGround(Ground *g, int shift) {
    if (!g || shift == 0) return;
    int newCols = g->cols + abs(shift);
     for(int i = 0; i < g->rows; i++) {
      g->data[i] = (char *)realloc(g->data[i], sizeof(char) * newCols);
      if (shift > 0) {
        memmove(g->data[i] + shift, g->data[i], g->cols * sizeof(char));
        for (int j = 0; j < shift; j++) {
          g->data[i][j] = '.';
        }
      } else {
          for(int j = g->cols -1; j >= 0; j--){
            g->data[i][j-shift] = g->data[i][j];
          }
           for (int j = 0; j < -shift; j++) {
            g->data[i][j] = '.';
        }
      }
    }
    g->cols = newCols;
}
int strToInt(const char *s) {
    int res = 0;
    int sign = 1;
    if (*s == '-') {
        sign = -1;
        s++;
    }
    while (isdigit(*s)) {
        res = res * 10 + (*s - '0');
        s++;
    }
    return res * sign;
}
void parseLine(const char *line, Ground *ground, int *maxX, int *minX, int *maxY, int *minY, int xOffset, int yOffset) {
  char *token;
  char temp[MAX_LINE_LENGTH];
  strcpy(temp, line);
  token = strtok(temp, "=, .");
  if (token == NULL) return;
  if (token[0] == 'x') {
    token = strtok(NULL, "=, .");
    int x = strToInt(token) - xOffset;
    token = strtok(NULL, "=, .");
     token = strtok(NULL, "=, .");
    int y1 = strToInt(token) - yOffset;
    token = strtok(NULL, "=, .");
    int y2 = strToInt(token) - yOffset;
      if (x >= *maxX) {
         expandGround(ground,ground->rows, x - *minX + 1);
        *maxX = x;
      }
      if (x < *minX) {
          shiftGround(ground, *minX - x);
          *minX = x;
      }

      if (y2 > *maxY) {
        expandGround(ground, y2+1,*maxX - *minX + 1);
        *maxY = y2;
      }
      if (y1 < *minY) {
        *minY = y1;
      }
      for (int i = y1; i <= y2; i++) {
        ground->data[i][x - *minX] = '#';
      }

  } else if (token[0] == 'y') {
    token = strtok(NULL, "=, .");
    int y = strToInt(token) - yOffset;
    token = strtok(NULL, "=, .");
    token = strtok(NULL, "=, .");
    int x1 = strToInt(token) - xOffset;
    token = strtok(NULL, "=, .");
    int x2 = strToInt(token) - xOffset;
        if (y > *maxY) {
           expandGround(ground,y+1, *maxX - *minX + 1);
          *maxY = y;
        }
        if (x2 >= *maxX) {
           expandGround(ground,ground->rows, x2 - *minX + 1);
          *maxX = x2;
        }
        if (x1 < *minX) {
          shiftGround(ground, *minX - x1);
          *minX = x1;
        }
        if (y < *minY) {
            *minY = y;
        }
      for (int i = x1; i <= x2; i++) {
        ground->data[y][i - *minX] = '#';
      }
  }
}
int simulateWater(Ground *ground, int minX, int minY, int maxX, int maxY) {
  int waterCount = 0;
  int flowCount = 0;
  int roundLimit = 200000;
  int x = -minX;
  int y = 1;
  int tryLeft = 0;
  while (ground->data[1][x] != '|' && waterCount < roundLimit) {
    int canMove = 1;
    x = -minX;
    y = 1;
    tryLeft = 0;
     while(canMove) {
      if (y + 1 > maxY || ground->data[y+1][x] == '|') {
        ground->data[y][x] = '|';
        canMove = 0;
        if (y >= minY)
            flowCount++;
      } else if (ground->data[y+1][x] == '.') {
          y++;
          tryLeft = 0;
      } else if (ground->data[y+1][x] == '#' || ground->data[y+1][x] == '~') {
         if ((tryLeft == 1 && ground->data[y][x-1] == '|') ||
              (tryLeft == 2 && ground->data[y][x+1] == '|') ||
              (ground->data[y][x+1] == '|' && ground->data[y][x-1] != '.') ||
              (ground->data[y][x+1] != '.' && ground->data[y][x-1] == '|') )
          {
            ground->data[y][x] = '|';
             flowCount++;
            canMove = 0;
             for (int i = x + 1; ground->data[y][i] == '~'; i++) {
              ground->data[y][i] = '|';
              waterCount--;
              flowCount++;
             }
            for (int i = x - 1; ground->data[y][i] == '~'; i--) {
              ground->data[y][i] = '|';
              waterCount--;
                flowCount++;
            }
          } else if ((tryLeft == 0 && ground->data[y][x-1] == '.') || (tryLeft == 1 && ground->data[y][x-1] == '.'))
           {
            x--;
            tryLeft = 1;
          } else if ((tryLeft == 0 && ground->data[y][x+1] == '.') || (tryLeft == 2 && ground->data[y][x+1] == '.')) {
              x++;
              tryLeft = 2;
          } else {
             canMove = 0;
              ground->data[y][x] = '~';
              waterCount++;
          }
      }
    }
  }
    return waterCount;
}

int main() {
  FILE *fp = fopen("input.txt", "r");
  if (fp == NULL) {
    perror("Error opening file");
    return 1;
  }
  Ground ground = createGround();
  int maxX = 0, minX = 0, maxY = 0, minY = 20;
  int xOffset = 500, yOffset = 0;

  char line[MAX_LINE_LENGTH];
  while (fgets(line, sizeof(line), fp) != NULL) {
    line[strcspn(line, "\n")] = 0;
     if (strlen(line) > 0)
      parseLine(line, &ground, &maxX, &minX, &maxY, &minY, xOffset, yOffset);
  }
  fclose(fp);

  int result = simulateWater(&ground, minX, minY, maxX, maxY);
  printf("%d\n", result);
  freeGround(&ground);
  return 0;
}
