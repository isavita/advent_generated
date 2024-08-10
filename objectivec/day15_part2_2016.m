#import <Foundation/Foundation.h>

@interface Disc : NSObject
@property (nonatomic) int totalPositions;
@property (nonatomic) int startPosition;
@end

@implementation Disc
@end

int main(int argc, char *argv[]) {
    NSError *error;
    NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    if (!input) {
        NSLog(@"%@", error);
        return 1;
    }

    NSArray *lines = [input componentsSeparatedByString:@"\n"];
    NSMutableArray *discs = [NSMutableArray array];
    NSRegularExpression *discRegex = [NSRegularExpression regularExpressionWithPattern:@"Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+)." options:0 error:&error];

    for (NSString *line in lines) {
        NSTextCheckingResult *match = [discRegex firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
        if (match) {
            Disc *disc = [[Disc alloc] init];
            disc.totalPositions = [[line substringWithRange:[match rangeAtIndex:2]] intValue];
            disc.startPosition = [[line substringWithRange:[match rangeAtIndex:3]] intValue];
            [discs addObject:disc];
        }
    }

    Disc *newDisc = [[Disc alloc] init];
    newDisc.totalPositions = 11;
    newDisc.startPosition = 0;
    [discs addObject:newDisc];

    int time = 0;
    while (1) {
        BOOL passed = YES;
        int sum = 0;
        for (int i = 0; i < discs.count; i++) {
            Disc *disc = discs[i];
            int position = (disc.startPosition + time + i + 1) % disc.totalPositions;
            sum += position;
        }
        if (sum != 0) {
            time++;
        } else {
            NSLog(@"%d", time);
            break;
        }
    }

    return 0;
}