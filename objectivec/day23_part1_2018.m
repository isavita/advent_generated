#import <Foundation/Foundation.h>

@interface Nanobot : NSObject
@property (nonatomic) NSInteger x, y, z, radius;
@end

@implementation Nanobot
@end

NSInteger manhattanDistance(Nanobot *a, Nanobot *b) {
    return labs(a.x - b.x) + labs(a.y - b.y) + labs(a.z - b.z);
}

NSInteger countNanobotsInRange(NSArray<Nanobot *> *nanobots, Nanobot *strongest) {
    NSInteger count = 0;
    for (Nanobot *nanobot in nanobots) {
        if (manhattanDistance(nanobot, strongest) <= strongest.radius) {
            count++;
        }
    }
    return count;
}

Nanobot *findStrongestNanobot(NSArray<Nanobot *> *nanobots) {
    Nanobot *strongest = nanobots[0];
    for (Nanobot *nanobot in nanobots) {
        if (nanobot.radius > strongest.radius) {
            strongest = nanobot;
        }
    }
    return strongest;
}

NSArray<Nanobot *> *parseNanobots(NSString *filePath) {
    NSMutableArray<Nanobot *> *nanobots = [NSMutableArray array];
    NSString *pattern = @"pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)";
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:pattern options:0 error:nil];
    
    NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    for (NSString *line in [content componentsSeparatedByString:@"\n"]) {
        NSTextCheckingResult *match = [regex firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
        if (match) {
            Nanobot *nanobot = [[Nanobot alloc] init];
            nanobot.x = [[line substringWithRange:[match rangeAtIndex:1]] integerValue];
            nanobot.y = [[line substringWithRange:[match rangeAtIndex:2]] integerValue];
            nanobot.z = [[line substringWithRange:[match rangeAtIndex:3]] integerValue];
            nanobot.radius = [[line substringWithRange:[match rangeAtIndex:4]] integerValue];
            [nanobots addObject:nanobot];
        }
    }
    return nanobots;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray<Nanobot *> *nanobots = parseNanobots(@"input.txt");
        Nanobot *strongest = findStrongestNanobot(nanobots);
        NSInteger inRangeCount = countNanobotsInRange(nanobots, strongest);
        NSLog(@"%ld", (long)inRangeCount);
    }
    return 0;
}