
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *directions = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!directions) return 1;

        NSMutableDictionary<NSString *, NSNumber *> *rooms = [NSMutableDictionary dictionary];
        int x = 0, y = 0, dist = 0;
        rooms[@"0,0"] = @(0);
        NSMutableArray<NSDictionary *> *stack = [NSMutableArray array];

        NSUInteger len = [directions length];
        for (NSUInteger i = 1; i < len - 1; ++i) {
            unichar c = [directions characterAtIndex:i];
            if (c == '(') {
                [stack addObject:@{@"x": @(x), @"y": @(y), @"dist": @(dist)}];
            } else if (c == '|') {
                NSDictionary *state = stack.lastObject;
                x = [state[@"x"] intValue];
                y = [state[@"y"] intValue];
                dist = [state[@"dist"] intValue];
            } else if (c == ')') {
                [stack removeLastObject];
            } else {
                switch (c) {
                    case 'N': y -= 1; break;
                    case 'E': x += 1; break;
                    case 'S': y += 1; break;
                    case 'W': x -= 1; break;
                    default: break;
                }
                dist += 1;
                NSString *key = [NSString stringWithFormat:@"%d,%d", x, y];
                NSNumber *prev = rooms[key];
                if (!prev || dist < [prev intValue]) rooms[key] = @(dist);
            }
        }

        NSInteger maxDoors = 0;
        long long roomsWith1000 = 0;
        for (NSNumber *value in rooms.allValues) {
            NSInteger d = [value integerValue];
            if (d > maxDoors) maxDoors = d;
            if (d >= 1000) roomsWith1000++;
        }

        printf("%lld\n%lld\n", (long long)maxDoors, roomsWith1000);
    }
    return 0;
}
