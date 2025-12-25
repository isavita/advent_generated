
#import <Foundation/Foundation.h>

static inline NSString *coordKey(int x, int y, int z) {
    return [NSString stringWithFormat:@"%d,%d,%d", x, y, z];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt"
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        if (!input) return 1;
        NSArray<NSString *> *lines = [input componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        if (lines.count == 0) return 1;
        NSMutableSet<NSString *> *active = [NSMutableSet set];

        for (int y = 0; y < lines.count; ++y) {
            NSString *line = lines[y];
            NSUInteger len = [line length];
            for (int x = 0; x < len; ++x) {
                unichar c = [line characterAtIndex:x];
                if (c == '#') {
                    [active addObject:coordKey(x, y, 0)];
                }
            }
        }

        static const int offsets[26][3] = {
            {-1,-1,-1},{-1,-1,0},{-1,-1,1},{-1,0,-1},{-1,0,0},{-1,0,1},{-1,1,-1},{-1,1,0},{-1,1,1},
            {0,-1,-1},{0,-1,0},{0,-1,1},{0,0,-1},{0,0,1},{0,1,-1},{0,1,0},{0,1,1},
            {1,-1,-1},{1,-1,0},{1,-1,1},{1,0,-1},{1,0,0},{1,0,1},{1,1,-1},{1,1,0},{1,1,1}
        };

        for (int cycle = 0; cycle < 6; ++cycle) {
            NSMutableSet<NSString *> *newActive = [NSMutableSet set];
            NSMutableDictionary<NSString *, NSNumber *> *inactiveCount = [NSMutableDictionary dictionary];

            for (NSString *key in active) {
                int x, y, z;
                sscanf([key UTF8String], "%d,%d,%d", &x, &y, &z);
                int neighbors = 0;

                for (int i = 0; i < 26; ++i) {
                    int nx = x + offsets[i][0];
                    int ny = y + offsets[i][1];
                    int nz = z + offsets[i][2];
                    NSString *nKey = coordKey(nx, ny, nz);
                    if ([active containsObject:nKey]) {
                        ++neighbors;
                    } else {
                        NSNumber *cnt = inactiveCount[nKey];
                        inactiveCount[nKey] = @([cnt unsignedIntegerValue] + 1);
                    }
                }

                if (neighbors == 2 || neighbors == 3) {
                    [newActive addObject:key];
                }
            }

            for (NSString *nKey in inactiveCount) {
                if ([inactiveCount[nKey] unsignedIntegerValue] == 3) {
                    [newActive addObject:nKey];
                }
            }

            active = newActive;
        }

        printf("%lu\n", (unsigned long)active.count);
    }
    return 0;
}
