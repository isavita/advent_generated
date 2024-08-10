#import <Foundation/Foundation.h>

// Define a structure to hold the valve information
@interface Valve : NSObject
@property (nonatomic, strong) NSString *name;
@property (nonatomic) NSInteger flowRate;
@property (nonatomic, strong) NSMutableSet<NSString *> *tunnels;
@end

@implementation Valve
@end

// Function to parse the input and create valves
NSArray<Valve *> *parseInput(NSString *filePath) {
    NSMutableArray<Valve *> *valves = [NSMutableArray array];
    NSError *error;
    NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        NSLog(@"Error reading file: %@", error);
        return nil;
    }

    NSArray<NSString *> *lines = [fileContents componentsSeparatedByString:@"\n"];
    for (NSString *line in lines) {
        NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"Valve ([A-Z]+) has flow rate=(\\d+); tunnels? leads? to valves? ([A-Z, ]+)" options:0 error:&error];
        NSArray<NSTextCheckingResult *> *matches = [regex matchesInString:line options:0 range:NSMakeRange(0, line.length)];
        if (matches.count > 0) {
            NSTextCheckingResult *match = matches[0];
            NSString *name = [line substringWithRange:[match rangeAtIndex:1]];
            NSInteger flowRate = [[line substringWithRange:[match rangeAtIndex:2]] integerValue];
            NSArray<NSString *> *tunnels = [[line substringWithRange:[match rangeAtIndex:3]] componentsSeparatedByString:@", "];

            Valve *valve = [[Valve alloc] init];
            valve.name = name;
            valve.flowRate = flowRate;
            valve.tunnels = [NSMutableSet setWithArray:tunnels];
            [valves addObject:valve];
        }
    }
    return valves;
}

// Function to find the maximum pressure released
NSInteger findMaxPressure(NSArray<Valve *> *valves, NSInteger timeLeft, NSString *currentValveName, NSInteger totalPressure, NSSet<NSString *> *openedValves, NSMutableDictionary *memo) {
    if (timeLeft <= 0) {
        return totalPressure;
    }

    NSString *key = [NSString stringWithFormat:@"%ld_%@_%@", (long)timeLeft, currentValveName, [[openedValves allObjects] componentsJoinedByString:@","]];
    if (memo[key]) {
        return [memo[key] integerValue];
    }

    NSInteger maxPressure = totalPressure;
    Valve *currentValve = nil;
    for (Valve *valve in valves) {
        if ([valve.name isEqualToString:currentValveName]) {
            currentValve = valve;
            break;
        }
    }

    if (![openedValves containsObject:currentValveName] && currentValve.flowRate > 0) {
        NSMutableSet<NSString *> *newOpenedValves = [NSMutableSet setWithSet:openedValves];
        [newOpenedValves addObject:currentValveName];
        NSInteger newPressure = totalPressure + (timeLeft - 1) * currentValve.flowRate;
        maxPressure = MAX(maxPressure, findMaxPressure(valves, timeLeft - 1, currentValveName, newPressure, newOpenedValves, memo));
    }

    for (NSString *tunnel in currentValve.tunnels) {
        maxPressure = MAX(maxPressure, findMaxPressure(valves, timeLeft - 1, tunnel, totalPressure, openedValves, memo));
    }

    memo[key] = @(maxPressure);
    return maxPressure;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray<Valve *> *valves = parseInput(@"input.txt");
        NSMutableDictionary *memo = [NSMutableDictionary dictionary];
        NSInteger maxPressure = findMaxPressure(valves, 30, @"AA", 0, [NSSet set], memo);
        NSLog(@"Max pressure released: %ld", (long)maxPressure);
    }
    return 0;
}