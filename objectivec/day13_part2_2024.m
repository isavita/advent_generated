
#import <Foundation/Foundation.h>

#define OFFSET 10000000000000LL

static BOOL parseCoords(NSString *line, long long *x, long long *y) {
    NSError *re1 = nil, *re2 = nil;
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"[-+]?\\d+"
                                                                        options:0
                                                                          error:&re1];
    NSArray *matches = [regex matchesInString:line options:0 range:NSMakeRange(0, line.length)];
    if (matches.count < 2) return NO;
    *x = [[line substringWithRange:[matches[0] range]] longLongValue];
    *y = [[line substringWithRange:[matches[1] range]] longLongValue];
    return YES;
}

static long long solveMachine(long long ax, long long ay,
                             long long bx, long long by,
                             long long px, long long py) {
    long long D  = ax * by - ay * bx;
    if (D == 0) return -1;
    long long numA = px * by - py * bx;
    long long numB = py * ax - px * ay;
    if (numA % D != 0 || numB % D != 0) return -1;
    long long a = numA / D;
    long long b = numB / D;
    if (a < 0 || b < 0) return -1;
    return 3 * a + b;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *text = [NSString stringWithContentsOfFile:path
                                               encoding:NSUTF8StringEncoding
                                                  error:nil];
        if (!text) {
            printf("0 0\n");
            return 1;
        }
        NSArray *lines = [text componentsSeparatedByCharactersInSet:
                         [NSCharacterSet newlineCharacterSet]];

        long long ax = 0, ay = 0, bx = 0, by = 0, px = 0, py = 0;
        long long totalCost = 0, solvedCount = 0;
        BOOL inBlock = NO;

        for (NSString *raw in lines) {
            NSString *line = [raw stringByTrimmingCharactersInSet:
                             [NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if (line.length == 0) {
                if (inBlock) {
                    long long cost = solveMachine(ax, ay, bx, by,
                                                  px + OFFSET, py + OFFSET);
                    if (cost >= 0) { solvedCount++; totalCost += cost; }
                    ax = ay = bx = by = px = py = 0;
                    inBlock = NO;
                }
                continue;
            }
            inBlock = YES;
            if ([line containsString:@"Button A:"] || [line containsString:@"A:"]) {
                parseCoords(line, &ax, &ay);
            } else if ([line containsString:@"Button B:"] || [line containsString:@"B:"]) {
                parseCoords(line, &bx, &by);
            } else if ([line containsString:@"Prize:"] || [line containsString:@"P:"]) {
                parseCoords(line, &px, &py);
            }
        }
        if (inBlock) {
            long long cost = solveMachine(ax, ay, bx, by, px + OFFSET, py + OFFSET);
            if (cost >= 0) { solvedCount++; totalCost += cost; }
        }
        printf("%lld %lld\n", solvedCount, totalCost);
    }
    return 0;
}
