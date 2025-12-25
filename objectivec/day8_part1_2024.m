
#include <Foundation/Foundation.h>

int main(int argc, char *argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
        if (!fileContents) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }
        NSArray *lines = [fileContents componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];

        if ([lines count] == 0) {
            printf("0\n");
            return 0;
        }

        NSUInteger h = [lines count];
        NSUInteger w = [[lines[0] isKindOfClass:[NSString class]] ? (id)lines[0] : @"" length];

        NSMutableDictionary *antennas = [NSMutableDictionary dictionary];

        for (NSUInteger y = 0; y < h; y++) {
            NSString *line = lines[y];
            for (NSUInteger x = 0; x < w; x++) {
                unichar c = [line characterAtIndex:x];
                if (c != '.') {
                    NSString *key = [NSString stringWithCharacters:&c length:1];
                    NSMutableArray *coords = antennas[key];
                    if (!coords) {
                        coords = [[NSMutableArray alloc] init];
                        antennas[key] = coords;
                    }
                    NSArray *coord = @[@(y), @(x)];
                    [coords addObject:coord];
                }
            }
        }

        NSMutableSet *antinodes = [NSMutableSet set];

        for (NSArray *coords in antennas.allValues) {
            NSUInteger n = coords.count;
            for (NSUInteger i = 0; i < n; i++) {
                for (NSUInteger j = i + 1; j < n; j++) {
                    NSArray *A = coords[i];
                    NSInteger Ay = [A[0] integerValue];
                    NSInteger Ax = [A[1] integerValue];
                    NSArray *B = coords[j];
                    NSInteger By = [B[0] integerValue];
                    NSInteger Bx = [B[1] integerValue];

                    NSInteger p1y = 2 * Ay - By;
                    NSInteger p1x = 2 * Ax - Bx;
                    NSInteger p2y = 2 * By - Ay;
                    NSInteger p2x = 2 * Bx - Ax;

                    if (p1y >= 0 && p1y < h && p1x >= 0 && p1x < w) {
                        [antinodes addObject:[NSString stringWithFormat:@"%ld,%ld", (long)p1y, (long)p1x]];
                    }
                    if (p2y >= 0 && p2y < h && p2x >= 0 && p2x < w) {
                        [antinodes addObject:[NSString stringWithFormat:@"%ld,%ld", (long)p2y, (long)p2x]];
                    }
                }
            }
        }

        printf("%lu\n", [antinodes count]);
    }
    return 0;
}
