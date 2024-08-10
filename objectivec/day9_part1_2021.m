#import <Foundation/Foundation.h>

BOOL isLowPoint(NSArray<NSArray<NSNumber *> *> *heightmap, NSInteger x, NSInteger y) {
    NSInteger height = [heightmap[y][x] integerValue];
    if (x > 0 && [heightmap[y][x - 1] integerValue] <= height) return NO;
    if (x < heightmap[y].count - 1 && [heightmap[y][x + 1] integerValue] <= height) return NO;
    if (y > 0 && [heightmap[y - 1][x] integerValue] <= height) return NO;
    if (y < heightmap.count - 1 && [heightmap[y + 1][x] integerValue] <= height) return NO;
    return YES;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) return 1;

        NSMutableArray<NSMutableArray<NSNumber *> *> *heightmap = [NSMutableArray array];
        for (NSString *line in [content componentsSeparatedByString:@"\n"]) {
            if (line.length == 0) continue;
            NSMutableArray<NSNumber *> *row = [NSMutableArray array];
            for (NSUInteger i = 0; i < line.length; i++) {
                [row addObject:@([[line substringWithRange:NSMakeRange(i, 1)] integerValue])];
            }
            [heightmap addObject:row];
        }

        NSInteger totalRiskLevel = 0;
        for (NSUInteger y = 0; y < heightmap.count; y++) {
            for (NSUInteger x = 0; x < heightmap[y].count; x++) {
                if (isLowPoint(heightmap, x, y)) {
                    totalRiskLevel += 1 + [heightmap[y][x] integerValue];
                }
            }
        }

        NSLog(@"%ld", (long)totalRiskLevel);
    }
    return 0;
}