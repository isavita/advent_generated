
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_LINE_LENGTH 256

typedef struct {
    char **data;
    int rows;
    int cols;
} Ground;

static Ground createGround(void) {
    Ground g;
    g.rows = 1;
    g.cols = 1;
    g.data = malloc(sizeof(char *) * g.rows);
    g.data[0] = malloc(sizeof(char) * g.cols);
    g.data[0][0] = '+';
    return g;
}

static void freeGround(Ground *g) {
    if (!g) return;
    for (int i = 0; i < g->rows; i++) free(g->data[i]);
    free(g->data);
    g->data = NULL;
    g->rows = g->cols = 0;
}

static void expandGround(Ground *g, int newRows, int newCols) {
    if (newRows > g->rows) {
        g->data = realloc(g->data, sizeof(char *) * newRows);
        for (int i = g->rows; i < newRows; i++) {
            g->data[i] = malloc(sizeof(char) * g->cols);
            memset(g->data[i], '.', g->cols);
        }
        g->rows = newRows;
    }
    if (newCols > g->cols) {
        for (int i = 0; i < g->rows; i++) {
            g->data[i] = realloc(g->data[i], sizeof(char) * newCols);
            memset(g->data[i] + g->cols, '.', newCols - g->cols);
        }
        g->cols = newCols;
    }
}

static void shiftGround(Ground *g, int shift) {
    if (!shift) return;
    int newCols = g->cols + abs(shift);
    for (int i = 0; i < g->rows; i++) {
        g->data[i] = realloc(g->data[i], sizeof(char) * newCols);
        if (shift > 0) {
            memmove(g->data[i] + shift, g->data[i], g->cols);
            memset(g->data[i], '.', shift);
        } else {
            for (int j = g->cols - 1; j >= 0; j--)
                g->data[i][j + shift] = g->data[i][j];
            memset(g->data[i], '.', -shift);
        }
    }
    g->cols = newCols;
}

static int strToInt(const char *s) {
    int v = 0, sign = 1;
    if (*s == '-') { sign = -1; s++; }
    while (isdigit(*s)) v = v * 10 + (*s++ - '0');
    return v * sign;
}

static void parseLine(const char *line, Ground *ground,
                      int *maxX, int *minX, int *maxY, int *minY,
                      int xOff, int yOff) {
    char tmp[MAX_LINE_LENGTH];
    strcpy(tmp, line);
    char *tok = strtok(tmp, "=, .");
    if (!tok) return;
    if (tok[0] == 'x') {
        int x = strToInt(strtok(NULL, "=, .")) - xOff;
        strtok(NULL, "=, .");
        int y1 = strToInt(strtok(NULL, "=, .")) - yOff;
        int y2 = strToInt(strtok(NULL, "=, .")) - yOff;
        if (x >= *maxX) {
            expandGround(ground, ground->rows, x - *minX + 1);
            *maxX = x;
        }
        if (x < *minX) {
            shiftGround(ground, *minX - x);
            *minX = x;
        }
        if (y2 > *maxY) {
            expandGround(ground, y2 + 1, *maxX - *minX + 1);
            *maxY = y2;
        }
        if (y1 < *minY) *minY = y1;
        for (int i = y1; i <= y2; i++) ground->data[i][x - *minX] = '#';
    } else {
        int y = strToInt(strtok(NULL, "=, .")) - yOff;
        strtok(NULL, "=, .");
        int x1 = strToInt(strtok(NULL, "=, .")) - xOff;
        int x2 = strToInt(strtok(NULL, "=, .")) - xOff;
        if (y > *maxY) {
            expandGround(ground, y + 1, *maxX - *minX + 1);
            *maxY = y;
        }
        if (x2 >= *maxX) {
            expandGround(ground, ground->rows, x2 - *minX + 1);
            *maxX = x2;
        }
        if (x1 < *minX) {
            shiftGround(ground, *minX - x1);
            *minX = x1;
        }
        if (y < *minY) *minY = y;
        for (int i = x1; i <= x2; i++) ground->data[y][i - *minX] = '#';
    }
}

static int simulateWater(Ground *ground, int minX, int minY, int maxX, int maxY) {
    int water = 0;
    int flow = 0;
    int limit = 200000;
    int startX = -minX;
    while (ground->data[1][startX] != '|' && water < limit) {
        int x = startX, y = 1, tryLeft = 0, moving = 1;
        while (moving) {
            if (y + 1 > maxY || ground->data[y + 1][x] == '|') {
                ground->data[y][x] = '|';
                moving = 0;
                if (y >= minY) flow++;
            } else if (ground->data[y + 1][x] == '.') {
                y++;
                tryLeft = 0;
            } else {
                if ((tryLeft == 1 && ground->data[y][x - 1] == '|') ||
                    (tryLeft == 2 && ground->data[y][x + 1] == '|') ||
                    (ground->data[y][x + 1] == '|' && ground->data[y][x - 1] != '.') ||
                    (ground->data[y][x + 1] != '.' && ground->data[y][x - 1] == '|')) {
                    ground->data[y][x] = '|';
                    flow++;
                    moving = 0;
                    for (int i = x + 1; ground->data[y][i] == '~'; i++) {
                        ground->data[y][i] = '|';
                        water--;
                        flow++;
                    }
                    for (int i = x - 1; ground->data[y][i] == '~'; i--) {
                        ground->data[y][i] = '|';
                        water--;
                        flow++;
                    }
                } else if ((tryLeft == 0 && ground->data[y][x - 1] == '.') ||
                           (tryLeft == 1 && ground->data[y][x - 1] == '.')) {
                    x--;
                    tryLeft = 1;
                } else if ((tryLeft == 0 && ground->data[y][x + 1] == '.') ||
                           (tryLeft == 2 && ground->data[y][x + 1] == '.')) {
                    x++;
                    tryLeft = 2;
                } else {
                    ground->data[y][x] = '~';
                    water++;
                    moving = 0;
                }
            }
        }
    }
    return water;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        Ground ground = createGround();
        int maxX = 0, minX = 0, maxY = 0, minY = 20;
        int xOff = 500, yOff = 0;
        for (NSString *line in [content componentsSeparatedByString:@"\n"]) {
            if (line.length == 0) continue;
            parseLine([line UTF8String], &ground, &maxX, &minX, &maxY, &minY, xOff, yOff);
        }
        int result = simulateWater(&ground, minX, minY, maxX, maxY);
        printf("%d\n", result);
        freeGround(&ground);
    }
    return 0;
}
