
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 0;
        NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableSet *blackTiles = [NSMutableSet set];

        for (NSString *line in lines) {
            if ([line length] == 0) continue;
            int x = 0, y = 0;
            NSUInteger i = 0;
            while (i < [line length]) {
                unichar c = [line characterAtIndex:i];
                if (c == 'e') { x++; i++; }
                else if (c == 'w') { x--; i++; }
                else if (c == 's') {
                    unichar n = [line characterAtIndex:i+1];
                    if (n == 'e') { y--; i += 2; }
                    else if (n == 'w') { x--; y--; i += 2; }
                }
                else if (c == 'n') {
                    unichar n = [line characterAtIndex:i+1];
                    if (n == 'w') { y++; i += 2; }
                    else if (n == 'e') { x++; y++; i += 2; }
                }
            }
            NSString *key = [NSString stringWithFormat:@"%d,%d", x, y];
            if ([blackTiles containsObject:key]) [blackTiles removeObject:key];
            else [blackTiles addObject:key];
        }

        const int dirs[6][2] = { {1,0}, {-1,0}, {0,-1}, {-1,-1}, {0,1}, {1,1} };

        for (int day = 0; day < 100; day++) {
            NSMutableSet *toCheck = [NSMutableSet set];
            for (NSString *k in blackTiles) {
                NSArray *parts = [k componentsSeparatedByString:@","];
                int x = [parts[0] intValue];
                int y = [parts[1] intValue];
                [toCheck addObject:k];
                for (int d = 0; d < 6; d++) {
                    int nx = x + dirs[d][0];
                    int ny = y + dirs[d][1];
                    [toCheck addObject:[NSString stringWithFormat:@"%d,%d", nx, ny]];
                }
            }

            NSMutableSet *newBlack = [NSMutableSet set];
            for (NSString *k in toCheck) {
                NSArray *parts = [k componentsSeparatedByString:@","];
                int x = [parts[0] intValue];
                int y = [parts[1] intValue];
                int count = 0;
                for (int d = 0; d < 6; d++) {
                    int nx = x + dirs[d][0];
                    int ny = y + dirs[d][1];
                    NSString *nk = [NSString stringWithFormat:@"%d,%d", nx, ny];
                    if ([blackTiles containsObject:nk]) count++;
                }
                BOOL isBlack = [blackTiles containsObject:k];
                if (isBlack && (count == 1 || count == 2)) [newBlack addObject:k];
                else if (!isBlack && count == 2) [newBlack addObject:k];
            }
            blackTiles = newBlack;
        }

        printf("%lu\n", (unsigned long)[blackTiles count]);
    }
    return 0;
}
