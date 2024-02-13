#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        NSMutableArray *stars = [NSMutableArray array];
        NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"position=<\\s*(-?\\d+),\\s*(-?\\d+)> velocity=<\\s*(-?\\d+),\\s*(-?\\d+)>" options:0 error:nil];
        
        for (NSString *line in lines) {
            NSTextCheckingResult *match = [regex firstMatchInString:line options:0 range:NSMakeRange(0, [line length])];
            if (match) {
                int x = [[line substringWithRange:[match rangeAtIndex:1]] intValue];
                int y = [[line substringWithRange:[match rangeAtIndex:2]] intValue];
                int vX = [[line substringWithRange:[match rangeAtIndex:3]] intValue];
                int vY = [[line substringWithRange:[match rangeAtIndex:4]] intValue];
                NSDictionary *star = @{@"x": @(x), @"y": @(y), @"vX": @(vX), @"vY": @(vY)};
                [stars addObject:star];
            }
        }
        
        int smallestT = 0;
        int smallestArea = INT_MAX;
        for (int t = 1; t < 100000; t++) {
            int maxX = 0;
            int maxY = 0;
            int minX = 0;
            int minY = 0;
            
            for (NSDictionary *star in stars) {
                int x = [star[@"x"] intValue] + [star[@"vX"] intValue] * t;
                if (maxX < x) {
                    maxX = x;
                } else if (minX > x) {
                    minX = x;
                }
                int y = [star[@"y"] intValue] + [star[@"vY"] intValue] * t;
                if (maxY < y) {
                    maxY = y;
                } else if (minY > y) {
                    minY = y;
                }
            }
            
            int lenX = maxX - minX + 1;
            int lenY = maxY - minY + 1;
            int area = lenX + lenY;
            
            if (smallestArea > area) {
                smallestArea = area;
                smallestT = t;
            }
        }
        printf("%d\n", smallestT);
    }
    return 0;
}