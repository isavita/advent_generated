
#import <Foundation/Foundation.h>

BOOL isSafeReport(int *levels, int len) {
    if (len < 2) return NO;
    int firstDiff = levels[1] - levels[0];
    if (firstDiff == 0) return NO;
    BOOL inc = firstDiff > 0;
    for (int i = 0; i < len - 1; i++) {
        int diff = levels[i + 1] - levels[i];
        if (diff == 0) return NO;
        if ((inc && diff <= 0) || (!inc && diff >= 0)) return NO;
        int ad = diff > 0 ? diff : -diff;
        if (ad < 1 || ad > 3) return NO;
    }
    return YES;
}

BOOL isSafeWithOneRemoval(int *levels, int len) {
    if (len <= 2) return NO;
    int tmp[100];
    for (int skip = 0; skip < len; skip++) {
        int tlen = 0;
        for (int i = 0; i < len; i++) if (i != skip) tmp[tlen++] = levels[i];
        if (isSafeReport(tmp, tlen)) return YES;
    }
    return NO;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *err = nil;
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
        if (!content) return 1;
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        int safeCount = 0;
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSArray<NSString *> *tokens = [line componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            int levels[100];
            int cnt = 0;
            for (NSString *t in tokens) if (t.length) levels[cnt++] = t.intValue;
            if (isSafeReport(levels, cnt) || isSafeWithOneRemoval(levels, cnt)) safeCount++;
        }
        printf("%d\n", safeCount);
    }
    return 0;
}
