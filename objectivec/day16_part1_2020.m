#import <Foundation/Foundation.h>

@interface Rule : NSObject

@property NSString *name;
@property NSArray<NSArray<NSNumber *> *> *ranges;

- (BOOL)isValid:(int)value;

@end

@implementation Rule

- (BOOL)isValid:(int)value {
    for (NSArray<NSNumber *> *rng in self.ranges) {
        if (value >= rng[0].intValue && value <= rng[1].intValue) {
            return YES;
        }
    }
    return NO;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *inputPath = @"input.txt";
        NSString *inputString = [NSString stringWithContentsOfFile:inputPath encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            printf("Error opening file: %s\n", error.localizedDescription.UTF8String);
            return 1;
        }
        
        NSArray<NSString *> *lines = [inputString componentsSeparatedByString:@"\n"];
        NSMutableArray<Rule *> *rules = [NSMutableArray array];
        BOOL scanningRules = YES;
        int errorRate = 0;
        
        NSRegularExpression *reRule = [NSRegularExpression regularExpressionWithPattern:@"^([^:]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$" options:0 error:nil];
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            if ([line hasPrefix:@"your ticket:"] || [line hasPrefix:@"nearby tickets:"]) {
                scanningRules = NO;
                continue;
            }
            if (scanningRules) {
                NSTextCheckingResult *match = [reRule firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
                if (match) {
                    NSString *name = [line substringWithRange:[match rangeAtIndex:1]];
                    NSArray<NSNumber *> *range1 = @[@([line substringWithRange:[match rangeAtIndex:2]].intValue), @([line substringWithRange:[match rangeAtIndex:3]].intValue)];
                    NSArray<NSNumber *> *range2 = @[@([line substringWithRange:[match rangeAtIndex:4]].intValue), @([line substringWithRange:[match rangeAtIndex:5]].intValue)];
                    Rule *rule = [[Rule alloc] init];
                    rule.name = name;
                    rule.ranges = @[range1, range2];
                    [rules addObject:rule];
                }
            } else {
                NSArray<NSString *> *values = [line componentsSeparatedByString:@","];
                for (NSString *value in values) {
                    int val = value.intValue;
                    BOOL valid = NO;
                    for (Rule *rule in rules) {
                        if ([rule isValid:val]) {
                            valid = YES;
                            break;
                        }
                    }
                    if (!valid) {
                        errorRate += val;
                    }
                }
            }
        }
        
        printf("%d\n", errorRate);
    }
    return 0;
}