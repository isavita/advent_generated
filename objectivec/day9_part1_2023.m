
#import <Foundation/Foundation.h>

static BOOL allZeros(int *nums, int size) {
    for (int i = 0; i < size; i++) if (nums[i] != 0) return NO;
    return YES;
}

static int calculateExtrapolation(int *history, int size, int *extrap) {
    for (int i = 1; i < size; i++) extrap[i - 1] = history[i] - history[i - 1];
    return size - 1;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        long long result = 0;
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSArray *tokens = [line componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            int history[128];
            int sizes[128];
            int series[128][128];
            int histSize = 0;
            for (NSString *t in tokens) {
                if (t.length) history[histSize++] = (int)[t integerValue];
            }
            memcpy(series[0], history, histSize * sizeof(int));
            sizes[0] = histSize;
            int levels = 1;
            for (int i = 1; i < histSize; i++) {
                if (allZeros(series[i - 1], sizes[i - 1])) break;
                sizes[i] = calculateExtrapolation(series[i - 1], sizes[i - 1], series[i]);
                levels++;
            }
            int future = 0;
            for (int i = levels - 1; i >= 0; i--) future += series[i][sizes[i] - 1];
            result += future;
        }
        printf("%lld\n", result);
    }
    return 0;
}
