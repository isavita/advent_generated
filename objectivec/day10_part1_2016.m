#import <Foundation/Foundation.h>

@interface Bot : NSObject
@property (nonatomic, strong) NSString *lowTo;
@property (nonatomic, strong) NSString *highTo;
@property (nonatomic, strong) NSMutableArray *chips;
@end

@implementation Bot
@end

int main(int argc, char *argv[]) {
    NSError *error;
    NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        NSLog(@"%@", error);
        return 1;
    }

    NSMutableDictionary *bots = [NSMutableDictionary dictionary];
    NSRegularExpression *valueRegex = [NSRegularExpression regularExpressionWithPattern:@"value (\\d+) goes to (bot \\d+)" options:0 error:&error];
    NSRegularExpression *givesRegex = [NSRegularExpression regularExpressionWithPattern:@"(bot \\d+) gives low to (bot \\d+|output \\d+) and high to (bot \\d+|output \\d+)" options:0 error:&error];

    NSArray *lines = [input componentsSeparatedByString:@"\n"];
    for (NSString *line in lines) {
        if ([valueRegex numberOfMatchesInString:line options:0 range:NSMakeRange(0, line.length)] > 0) {
            NSTextCheckingResult *match = [valueRegex firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
            NSString *value = [line substringWithRange:[match rangeAtIndex:1]];
            NSString *botID = [line substringWithRange:[match rangeAtIndex:2]];

            if (!bots[botID]) {
                bots[botID] = [[Bot alloc] init];
            }
            [(Bot *)bots[botID] setChips:[NSMutableArray arrayWithObject:[value intValue]]];

        } else if ([givesRegex numberOfMatchesInString:line options:0 range:NSMakeRange(0, line.length)] > 0) {
            NSTextCheckingResult *match = [givesRegex firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
            NSString *botID = [line substringWithRange:[match rangeAtIndex:1]];
            NSString *lowTo = [line substringWithRange:[match rangeAtIndex:2]];
            NSString *highTo = [line substringWithRange:[match rangeAtIndex:3]];

            if (!bots[botID]) {
                bots[botID] = [[Bot alloc] init];
            }
            [(Bot *)bots[botID] setLowTo:lowTo];
            [(Bot *)bots[botID] setHighTo:highTo];
        }
    }

    while (1) {
        BOOL action = NO;
        for (NSString *botID in bots) {
            Bot *b = bots[botID];
            if (b.chips.count == 2) {
                action = YES;
                int low = MIN([b.chips[0] intValue], [b.chips[1] intValue]);
                int high = MAX([b.chips[0] intValue], [b.chips[1] intValue]);
                if (low == 17 && high == 61) {
                    NSLog(@"%@", botID);
                    return 0;
                }
                b.chips = nil;

                giveChip(bots, b.lowTo, low);
                giveChip(bots, b.highTo, high);
            }
        }
        if (!action) {
            break;
        }
    }

    return 0;
}

void giveChip(NSMutableDictionary *bots, NSString *target, int value) {
    if (!bots[target]) {
        bots[target] = [[Bot alloc] init];
    }
    [(Bot *)bots[target] setChips:[NSMutableArray arrayWithObject:[NSNumber numberWithInt:value]]];
}