#import <Foundation/Foundation.h>

@interface GuardRecord : NSObject
@property (nonatomic, assign) NSInteger guardID;
@property (nonatomic, strong) NSMutableDictionary<NSNumber*, NSNumber*> *sleepMinutes;
@end

@implementation GuardRecord
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContent componentsSeparatedByString:@"\n"];

        NSMutableArray *sortedRecords = [NSMutableArray array];
        for (NSString *line in lines) {
            if (line.length > 0) {
                [sortedRecords addObject:line];
            }
        }

        [sortedRecords sortUsingComparator:^NSComparisonResult(NSString *obj1, NSString *obj2) {
            return [obj1 compare:obj2];
        }];

        NSMutableDictionary<NSNumber*, GuardRecord*> *guards = [NSMutableDictionary dictionary];
        GuardRecord *currentGuard = nil;
        NSInteger sleepStart = -1;

        for (NSString *record in sortedRecords) {
            NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"\\[.+\\] Guard #(\\d+) begins shift" options:0 error:nil];
            NSTextCheckingResult *match = [regex firstMatchInString:record options:0 range:NSMakeRange(0, [record length])];
            if (match) {
                NSRange guardIDRange = [match rangeAtIndex:1];
                NSString *guardIDStr = [record substringWithRange:guardIDRange];
                NSInteger guardID = [guardIDStr integerValue];
                currentGuard = guards[@(guardID)];
                if (!currentGuard) {
                    currentGuard = [[GuardRecord alloc] init];
                    currentGuard.guardID = guardID;
                    currentGuard.sleepMinutes = [NSMutableDictionary dictionary];
                    guards[@(guardID)] = currentGuard;
                }
            } else if ([record containsString:@"falls asleep"]) {
                NSRegularExpression *minuteRegex = [NSRegularExpression regularExpressionWithPattern:@":(\\d{2})\\] falls asleep" options:0 error:nil];
                NSTextCheckingResult *minuteMatch = [minuteRegex firstMatchInString:record options:0 range:NSMakeRange(0, [record length])];
                NSRange minuteRange = [minuteMatch rangeAtIndex:1];
                NSString *minuteStr = [record substringWithRange:minuteRange];
                sleepStart = [minuteStr integerValue];
            } else if ([record containsString:@"wakes up"]) {
                NSRegularExpression *minuteRegex = [NSRegularExpression regularExpressionWithPattern:@":(\\d{2})\\] wakes up" options:0 error:nil];
                NSTextCheckingResult *minuteMatch = [minuteRegex firstMatchInString:record options:0 range:NSMakeRange(0, [record length])];
                NSRange minuteRange = [minuteMatch rangeAtIndex:1];
                NSString *minuteStr = [record substringWithRange:minuteRange];
                NSInteger wakeUp = [minuteStr integerValue];
                for (NSInteger minute = sleepStart; minute < wakeUp; minute++) {
                    NSNumber *count = currentGuard.sleepMinutes[@(minute)];
                    currentGuard.sleepMinutes[@(minute)] = @([count integerValue] + 1);
                }
            }
        }

        GuardRecord *maxSleepGuard = nil;
        NSInteger maxSleepMinutes = 0;
        NSInteger chosenMinute = 0;

        for (GuardRecord *guard in [guards allValues]) {
            NSInteger totalSleepMinutes = 0;
            NSInteger maxMinute = 0;
            NSInteger maxMinuteCount = 0;

            for (NSNumber *minute in guard.sleepMinutes) {
                NSInteger count = [guard.sleepMinutes[minute] integerValue];
                totalSleepMinutes += count;
                if (count > maxMinuteCount) {
                    maxMinuteCount = count;
                    maxMinute = [minute integerValue];
                }
            }

            if (totalSleepMinutes > maxSleepMinutes) {
                maxSleepMinutes = totalSleepMinutes;
                maxSleepGuard = guard;
                chosenMinute = maxMinute;
            }
        }

        NSInteger result = maxSleepGuard.guardID * chosenMinute;
        NSLog(@"The ID of the guard multiplied by the minute is: %ld", (long)result);
    }
    return 0;
}