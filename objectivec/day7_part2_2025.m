
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        if (!content) {
            printf("0\n");
            return 0;
        }
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        
        char grid[1000][1000];
        int height = 0, width = 0;
        for (NSString *line in lines) {
            NSString *trimmed = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if ([trimmed length] == 0) continue;
            if (width == 0) width = (int)[trimmed length];
            for (int i = 0; i < width; i++) {
                grid[height][i] = [trimmed characterAtIndex:i];
            }
            height++;
        }
        
        if (height == 0) {
            printf("0\n");
            return 0;
        }
        
        int startX = -1, startY = -1;
        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                if (grid[y][x] == 'S') {
                    startX = x;
                    startY = y;
                    goto found;
                }
            }
        }
    found:;
        if (startX == -1) {
            printf("0\n");
            return 0;
        }
        
        NSMutableDictionary *counts = [NSMutableDictionary dictionary];
        counts[@(startX)] = @1;
        
        for (int y = startY; y < height; y++) {
            NSMutableDictionary *next = [NSMutableDictionary dictionary];
            for (NSNumber *kx in counts) {
                int x = [kx intValue];
                unsigned long long cnt = [counts[kx] unsignedLongLongValue];
                if (x >= 0 && x < width && grid[y][x] == '^') {
                    NSNumber *left = @(x - 1);
                    NSNumber *right = @(x + 1);
                    next[left] = @([next[left] unsignedLongLongValue] + cnt);
                    next[right] = @([next[right] unsignedLongLongValue] + cnt);
                } else {
                    NSNumber *key = @(x);
                    next[key] = @([next[key] unsignedLongLongValue] + cnt);
                }
            }
            counts = next;
        }
        
        unsigned long long total = 0;
        for (NSNumber *v in [counts allValues]) {
            total += [v unsignedLongLongValue];
        }
        printf("%llu\n", total);
    }
    return 0;
}
