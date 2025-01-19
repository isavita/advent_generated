
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

typedef struct {
    int* rows;
    int* cols;
    int numRows;
    int numCols;
} Mirror;

Mirror parseMirror(char** mirrorStr, int numLines, int lineLength) {
    Mirror mirror;
    mirror.numRows = numLines;
    mirror.numCols = lineLength;
    mirror.rows = (int*)malloc(sizeof(int) * numLines);
    mirror.cols = (int*)malloc(sizeof(int) * lineLength);

    for(int i = 0; i < numLines; i++) mirror.rows[i] = 0;
    for(int i = 0; i < lineLength; i++) mirror.cols[i] = 0;
    
    for (int y = 0; y < numLines; y++) {
        for (int x = 0; x < lineLength; x++) {
            mirror.rows[y] <<= 1;
            mirror.cols[x] <<= 1;
            if (mirrorStr[y][x] == '#') {
                mirror.rows[y]++;
                mirror.cols[x]++;
            }
        }
    }

    return mirror;
}


int getMirrorAxisWithOneSmudge(int* lines, int numLines) {
    for (int i = 1; i < numLines; i++) {
        int numSmudges = 0;
        bool isMirror = true;
        int minLen = (i < numLines - i) ? i : numLines - i;
        for (int j = 0; j < minLen; j++) {
           if(lines[i-1-j] != lines[i+j]){
               int diff = lines[i-1-j] ^ lines[i+j];
               if(numSmudges > 0 || (diff & (diff-1)) !=0) {
                    isMirror = false;
                   break;
               }else {
                   numSmudges++;
               }
           }
        }
         if (isMirror && numSmudges == 1) {
                return i;
            }

    }
    return 0;
}


int solve(char** input, int numLines) {
    int res = 0;
    char** mirrorStr = (char**)malloc(sizeof(char*) * numLines);
    int mirrorStrIndex = 0;
    int currentMirrorLines = 0;
    int lineLength = 0;

    for(int i=0; i< numLines; i++){
        if(strlen(input[i]) == 0){
            if(currentMirrorLines >0){
                Mirror mirror = parseMirror(mirrorStr, currentMirrorLines, lineLength);
                 res += getMirrorAxisWithOneSmudge(mirror.cols, mirror.numCols);
                 res += getMirrorAxisWithOneSmudge(mirror.rows, mirror.numRows) * 100;
                 free(mirror.cols);
                 free(mirror.rows);
                 currentMirrorLines = 0;
            }
            mirrorStrIndex = 0;
           
        }else {
             mirrorStr[mirrorStrIndex++] = input[i];
              lineLength = strlen(input[i]);
             currentMirrorLines++;
        }
    }
    
    if(currentMirrorLines >0){
        Mirror mirror = parseMirror(mirrorStr, currentMirrorLines, lineLength);
        res += getMirrorAxisWithOneSmudge(mirror.cols, mirror.numCols);
        res += getMirrorAxisWithOneSmudge(mirror.rows, mirror.numRows) * 100;
         free(mirror.cols);
         free(mirror.rows);
    }
    free(mirrorStr);

    return res;
}



int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    char** input = NULL;
    size_t linecap = 0;
    char* line = NULL;
    int numLines = 0;

    while (getline(&line, &linecap, file) > 0) {
        input = (char**)realloc(input, sizeof(char*) * (numLines + 1));
        line[strcspn(line, "\r\n")] = 0;
        input[numLines] = strdup(line);
        numLines++;
    }

    if (line) free(line);
    fclose(file);

    printf("%d\n", solve(input, numLines));

    for (int i = 0; i < numLines; i++) {
        free(input[i]);
    }
    free(input);

    return 0;
}
