
#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <stdbool.h>

typedef struct {
    int *rows;
    int *cols;
    int numRows;
    int numCols;
} Mirror;

static Mirror parseMirror(char **src, int rows, int cols) {
    Mirror m;
    m.numRows = rows;
    m.numCols = cols;
    m.rows = calloc(rows, sizeof(int));
    m.cols = calloc(cols, sizeof(int));
    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) {
            m.rows[y] <<= 1;
            m.cols[x] <<= 1;
            if (src[y][x] == '#') {
                m.rows[y]++; m.cols[x]++;
            }
        }
    }
    return m;
}

static int getMirrorAxisWithOneSmudge(int *lines, int n) {
    for (int i = 1; i < n; i++) {
        int smudges = 0;
        bool ok = true;
        int min = i < n - i ? i : n - i;
        for (int j = 0; j < min; j++) {
            if (lines[i - 1 - j] != lines[i + j]) {
                int diff = lines[i - 1 - j] ^ lines[i + j];
                if (smudges || (diff & (diff - 1))) { ok = false; break; }
                smudges = 1;
            }
        }
        if (ok && smudges == 1) return i;
    }
    return 0;
}

static int solve(NSArray<NSString *> *input) {
    int res = 0;
    int total = (int)input.count;
    char **block = malloc(sizeof(char *) * total);
    int blkIdx = 0, blkLines = 0, lineLen = 0;
    for (int i = 0; i < total; i++) {
        NSString *s = input[i];
        if (s.length == 0) {
            if (blkLines) {
                Mirror m = parseMirror(block, blkLines, lineLen);
                res += getMirrorAxisWithOneSmudge(m.cols, m.numCols);
                res += getMirrorAxisWithOneSmudge(m.rows, m.numRows) * 100;
                free(m.cols); free(m.rows);
                blkLines = 0; blkIdx = 0;
            }
        } else {
            block[blkIdx++] = (char *)[s UTF8String];
            lineLen = (int)s.length;
            blkLines++;
        }
    }
    if (blkLines) {
        Mirror m = parseMirror(block, blkLines, lineLen);
        res += getMirrorAxisWithOneSmudge(m.cols, m.numCols);
        res += getMirrorAxisWithOneSmudge(m.rows, m.numRows) * 100;
        free(m.cols); free(m.rows);
    }
    free(block);
    return res;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *data = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [data componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        printf("%d\n", solve(lines));
    }
    return 0;
}
