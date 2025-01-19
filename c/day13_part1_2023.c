
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

#define MAX_LINES 150
#define MAX_LINE_LEN 150

typedef struct {
    int rows[MAX_LINES];
    int cols[MAX_LINE_LEN];
    int numRows;
    int numCols;
} Mirror;

Mirror mirrors[MAX_LINES];
int numMirrors = 0;

void parseMirror(char mirrorStr[MAX_LINES][MAX_LINE_LEN], int numLines) {
    Mirror *mirror = &mirrors[numMirrors++];
    mirror->numRows = numLines;
    mirror->numCols = strlen(mirrorStr[0]);

    for (int y = 0; y < mirror->numRows; y++) {
        mirror->rows[y] = 0;
        for (int x = 0; x < mirror->numCols; x++) {
            mirror->rows[y] <<= 1;
            if (mirrorStr[y][x] == '#') {
                mirror->rows[y]++;
            }
        }
    }

    for(int x = 0; x < mirror->numCols; x++){
        mirror->cols[x] = 0;
        for(int y = 0; y < mirror->numRows; y++){
            mirror->cols[x] <<= 1;
            if (mirrorStr[y][x] == '#'){
                mirror->cols[x]++;
            }
        }
    }
}

int getMirrorAxis(int lines[], int numLines) {
    for (int i = 1; i < numLines; i++) {
        bool isMirror = true;
        for (int j = 0; isMirror && j < fmin(i, numLines - i); j++) {
            if (lines[i - 1 - j] != lines[i + j]) {
                isMirror = false;
            }
        }
        if (isMirror) {
            return i;
        }
    }
    return 0;
}

int getMirrorAxisWithOneSmudge(int lines[], int numLines) {
    for (int i = 1; i < numLines; i++) {
        bool isMirror = true;
        int numSmudges = 0;
        for (int j = 0; isMirror && j < fmin(i, numLines - i); j++) {
           if (lines[i - 1 - j] != lines[i + j]) {
                if (numSmudges > 0) {
                    isMirror = false;
                } else {
                    int dif = lines[i - 1 - j] ^ lines[i + j];
                    if ((dif & (dif - 1)) == 0) {
                        numSmudges++;
                    } else {
                         isMirror = false;
                    }
                }
            }
        }
        if (isMirror && numSmudges == 1) {
            return i;
        }
    }
    return 0;
}

int solve() {
    int res = 0;
    for (int i = 0; i < numMirrors; i++) {
        res += getMirrorAxis(mirrors[i].cols, mirrors[i].numCols);
        res += getMirrorAxis(mirrors[i].rows, mirrors[i].numRows) * 100;
    }
    return res;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[MAX_LINE_LEN];
    char mirrorStr[MAX_LINES][MAX_LINE_LEN];
    int numLines = 0;

    while (fgets(line, sizeof(line), file) != NULL) {
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0') {
            if (numLines > 0){
                parseMirror(mirrorStr, numLines);
                numLines = 0;
            }
        } else {
            
            size_t len = strlen(line);
            if (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
                line[len-1] = '\0';
            }

            strcpy(mirrorStr[numLines++], line);
        }
    }

    if (numLines > 0) {
        parseMirror(mirrorStr, numLines);
    }


    fclose(file);
    printf("%d\n", solve());
    return 0;
}
