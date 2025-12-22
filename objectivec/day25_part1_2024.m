
#import <Foundation/Foundation.h>

#define MAX_LINE_LEN 256
#define MAX_ITEMS 1000
#define BLOCK_HEIGHT 7
#define SHAPE_WIDTH 5
#define SHAPE_HEIGHT 6

static int locks[MAX_ITEMS][SHAPE_WIDTH];
static int keys[MAX_ITEMS][SHAPE_WIDTH];
static int numLocks = 0, numKeys = 0;

static inline int effectiveLength(NSString *s) {
    NSCharacterSet *trim = [NSCharacterSet whitespaceAndNewlineCharacterSet];
    return (int)[[s stringByTrimmingCharactersInSet:trim] length];
}

static void parseLock(char b[BLOCK_HEIGHT][MAX_LINE_LEN], int out[SHAPE_WIDTH]) {
    for (int c = 0; c < SHAPE_WIDTH; ++c) {
        int cnt = 0;
        for (int r = 1; r < BLOCK_HEIGHT; ++r) {
            if (b[r][c] == '#') ++cnt; else break;
        }
        out[c] = cnt;
    }
}
static void parseKey(char b[BLOCK_HEIGHT][MAX_LINE_LEN], int out[SHAPE_WIDTH]) {
    for (int c = 0; c < SHAPE_WIDTH; ++c) {
        int cnt = 0;
        for (int r = SHAPE_HEIGHT - 1; r >= 0; --r) {
            if (b[r][c] == '#') ++cnt; else break;
        }
        out[c] = cnt;
    }
}
static inline BOOL fits(const int lock[SHAPE_WIDTH], const int key[SHAPE_WIDTH]) {
    for (int i = 0; i < SHAPE_WIDTH; ++i)
        if (lock[i] + key[i] > SHAPE_HEIGHT - 1) return NO;
    return YES;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!content) { printf("0\n"); return 1; }

        NSArray<NSString *> *rawLines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        char block[BLOCK_HEIGHT][MAX_LINE_LEN];
        int lineIdx = 0, totalLines = 0;
        BOOL blockInvalid = NO;

        for (NSString *raw in rawLines) {
            int len = effectiveLength(raw);
            if (len == 0) continue;
            ++totalLines;
            NSString *line = [raw stringByAppendingString:@"\n"];
            strncpy(block[lineIdx], [line UTF8String], MAX_LINE_LEN - 1);
            block[lineIdx][MAX_LINE_LEN - 1] = '\0';
            if (len < SHAPE_WIDTH) blockInvalid = YES;
            if (++lineIdx == BLOCK_HEIGHT) {
                if (!blockInvalid) {
                    BOOL isLock = YES;
                    for (int i = 0; i < SHAPE_WIDTH; ++i)
                        if (block[0][i] != '#') { isLock = NO; break; }
                    if (isLock) {
                        if (numLocks < MAX_ITEMS) parseLock(block, locks[numLocks++]);
                    } else {
                        if (numKeys < MAX_ITEMS) parseKey(block, keys[numKeys++]);
                    }
                }
                lineIdx = 0;
                blockInvalid = NO;
            }
        }

        if (totalLines == 0 || totalLines % BLOCK_HEIGHT != 0) { printf("0\n"); return 0; }

        int count = 0;
        for (int i = 0; i < numLocks; ++i)
            for (int j = 0; j < numKeys; ++j)
                if (fits(locks[i], keys[j])) ++count;

        printf("%d\n", count);
    }
    return 0;
}
