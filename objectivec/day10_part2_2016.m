#import <Foundation/Foundation.h>

@interface Bot : NSObject
@property (nonatomic, strong) NSString *lowTo, *highTo;
@property (nonatomic, strong) NSMutableArray<NSNumber *> *chips;
@end

@implementation Bot
- (instancetype)init {
    self = [super init];
    if (self) {
        _chips = [NSMutableArray array];
    }
    return self;
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSMutableDictionary<NSString *, Bot *> *bots = [NSMutableDictionary dictionary];
        NSMutableDictionary<NSString *, NSNumber *> *outputs = [NSMutableDictionary dictionary];
        
        NSString *filePath = @"input.txt";
        NSString *input = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [input componentsSeparatedByString:@"\n"];
        
        NSRegularExpression *valueRegex = [NSRegularExpression regularExpressionWithPattern:@"value (\\d+) goes to (bot \\d+)" options:0 error:nil];
        NSRegularExpression *givesRegex = [NSRegularExpression regularExpressionWithPattern:@"(bot \\d+) gives low to (bot \\d+|output \\d+) and high to (bot \\d+|output \\d+)" options:0 error:nil];
        
        for (NSString *line in lines) {
            if ([line length] == 0) continue;
            NSTextCheckingResult *valueMatch = [valueRegex firstMatchInString:line options:0 range:NSMakeRange(0, [line length])];
            if (valueMatch) {
                NSInteger value = [[line substringWithRange:[valueMatch rangeAtIndex:1]] integerValue];
                NSString *botID = [line substringWithRange:[valueMatch rangeAtIndex:2]];
                Bot *bot = bots[botID] ?: [[Bot alloc] init];
                [bot.chips addObject:@(value)];
                bots[botID] = bot;
            } else {
                NSTextCheckingResult *givesMatch = [givesRegex firstMatchInString:line options:0 range:NSMakeRange(0, [line length])];
                if (givesMatch) {
                    NSString *botID = [line substringWithRange:[givesMatch rangeAtIndex:1]];
                    NSString *lowTo = [line substringWithRange:[givesMatch rangeAtIndex:2]];
                    NSString *highTo = [line substringWithRange:[givesMatch rangeAtIndex:3]];
                    Bot *bot = bots[botID] ?: [[Bot alloc] init];
                    bot.lowTo = lowTo;
                    bot.highTo = highTo;
                    bots[botID] = bot;
                }
            }
        }

        while (YES) {
            BOOL action = NO;
            for (Bot *b in bots.allValues) {
                if (b.chips.count == 2) {
                    action = YES;
                    NSInteger low = MIN([b.chips[0] integerValue], [b.chips[1] integerValue]);
                    NSInteger high = MAX([b.chips[0] integerValue], [b.chips[1] integerValue]);
                    [b.chips removeAllObjects];
                    if ([b.lowTo hasPrefix:@"bot"]) {
                        Bot *lowBot = bots[b.lowTo] ?: [[Bot alloc] init];
                        [lowBot.chips addObject:@(low)];
                        bots[b.lowTo] = lowBot;
                    } else {
                        outputs[b.lowTo] = @(low);
                    }
                    if ([b.highTo hasPrefix:@"bot"]) {
                        Bot *highBot = bots[b.highTo] ?: [[Bot alloc] init];
                        [highBot.chips addObject:@(high)];
                        bots[b.highTo] = highBot;
                    } else {
                        outputs[b.highTo] = @(high);
                    }
                }
            }
            if (!action) break;
        }

        NSInteger result = [outputs[@"output 0"] integerValue] * [outputs[@"output 1"] integerValue] * [outputs[@"output 2"] integerValue];
        NSLog(@"%ld", (long)result);
    }
    return 0;
}