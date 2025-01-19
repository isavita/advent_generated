
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct {
    int i, j;
} Position;

Position findPosition(char **mat, int rows, int cols, char ch) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            if (mat[i][j] == ch) {
                Position pos;
                pos.i = i;
                pos.j = j;
                return pos;
            }
        }
    }
    Position pos;
    pos.i = -1;
    pos.j = -1;
    return pos;
}

bool ok(char **mat, int rows, int cols, Position st, char *seq) {
    Position curr = st;
    for (int i = 0; seq[i] != '\0'; i++) {
        if (mat[curr.i][curr.j] == ' ') {
            return false;
        }
        char ch = seq[i];
        switch (ch) {
            case '^':
                curr.i--;
                break;
            case 'v':
                curr.i++;
                break;
            case '<':
                curr.j--;
                break;
            case '>':
                curr.j++;
                break;
        }
        if (curr.i < 0 || curr.i >= rows || curr.j < 0 || curr.j >= cols) {
            return false;
        }
    }
    return true;
}

char* generateMoves(Position position, char objective, char **pad, int padRows, int padCols) {
    Position objPos = findPosition(pad, padRows, padCols, objective);
    char *ret = (char*)malloc(100 * sizeof(char));
    int retLen = 0;

    if (position.j > objPos.j) {
        for (int i = 0; i < position.j - objPos.j; i++){
            ret[retLen++] = '<';
        }
    }
    if (position.i > objPos.i) {
       for (int i = 0; i < position.i - objPos.i; i++){
            ret[retLen++] = '^';
        }
    }
    if (position.i < objPos.i) {
         for (int i = 0; i < objPos.i - position.i; i++){
            ret[retLen++] = 'v';
        }
    }
    if (position.j < objPos.j) {
        for (int i = 0; i < objPos.j - position.j; i++){
            ret[retLen++] = '>';
        }
    }
    ret[retLen] = '\0';
     if (!ok(pad, padRows, padCols, position, ret)) {
        retLen = 0;
        if (position.j < objPos.j) {
           for (int i = 0; i < objPos.j - position.j; i++){
                ret[retLen++] = '>';
            }
        }
         if (position.i > objPos.i) {
            for (int i = 0; i < position.i - objPos.i; i++){
                ret[retLen++] = '^';
            }
        }
        if (position.i < objPos.i) {
            for (int i = 0; i < objPos.i - position.i; i++){
                ret[retLen++] = 'v';
            }
        }
        if (position.j > objPos.j) {
           for (int i = 0; i < position.j - objPos.j; i++){
                ret[retLen++] = '<';
            }
        }
        ret[retLen] = '\0';
    }

    return ret;
}

int solve(char *code, int robots, char **keyPad, int keyPadRows, int keyPadCols, char **robotPad, int robotPadRows, int robotPadCols, int maxRobots) {
    if (robots <= 0) {
        return strlen(code);
    }

    int ret = 0;
    int posi = 3, posj = 2;
    if (robots != maxRobots) {
        posi = 0;
    }
     char *moves;

    for (int i = 0; code[i] != '\0'; i++) {
        char ch = code[i];
        if (robots == maxRobots) {
            moves = generateMoves((Position){posi, posj}, ch, keyPad, keyPadRows, keyPadCols);
            Position pos = findPosition(keyPad, keyPadRows, keyPadCols, ch);
            posi = pos.i;
            posj = pos.j;
        } else {
            moves = generateMoves((Position){posi, posj}, ch, robotPad, robotPadRows, robotPadCols);
            Position pos = findPosition(robotPad, robotPadRows, robotPadCols, ch);
            posi = pos.i;
            posj = pos.j;
        }
        int movesLen = strlen(moves);
        char *newCode = (char*)malloc((strlen(moves) + 2) * sizeof(char));
        strcpy(newCode, moves);
        strcat(newCode,"A");

        ret += solve(newCode, robots - 1, keyPad, keyPadRows, keyPadCols, robotPad, robotPadRows, robotPadCols, maxRobots);
        free(moves);
        free(newCode);
    }
    return ret;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int maxRobots = 3;
    char *keyPad[] = {
        "789",
        "456",
        "123",
        " 0A"
    };
    int keyPadRows = 4;
    int keyPadCols = 3;
    char *robotPad[] = {
        " ^A",
        "<v>"
    };
    int robotPadRows = 2;
    int robotPadCols = 3;

    int ret = 0;
    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    while ((read = getline(&line, &len, fp)) != -1) {
        char *code = strtok(line, "\n");
        if (code == NULL) continue;
       
        int numericPart = 0;
        for(int i=0; code[i] != '\0'; ++i) {
            if(code[i] >= '0' && code[i] <= '9') {
                numericPart = numericPart * 10 + (code[i] - '0');
            }
        }
        
        int sv = solve(code, maxRobots, keyPad, keyPadRows, keyPadCols, robotPad, robotPadRows, robotPadCols, maxRobots);
         ret += sv * numericPart;
    }

    printf("%d\n", ret);

    if(line) free(line);
    fclose(fp);
    return 0;
}
