#import <Foundation/Foundation.h>

NSDictionary *readRules(NSString *filePath) {
    NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    NSArray *lines = [content componentsSeparatedByString:@"\n"];
    NSMutableDictionary *rules = [NSMutableDictionary dictionary];
    for (NSString *line in lines) {
        if ([line containsString:@"->"]) {
            NSArray *parts = [line componentsSeparatedByString:@" -> "];
            rules[parts[0]] = parts[1];
        }
    }
    return rules;
}

NSString *applyInsertion(NSString *polymer, NSDictionary *rules) {
    NSMutableString *newPolymer = [NSMutableString string];
    for (NSUInteger i = 0; i < polymer.length - 1; i++) {
        [newPolymer appendFormat:@"%C", [polymer characterAtIndex:i]];
        NSString *pair = [polymer substringWithRange:NSMakeRange(i, 2)];
        if (rules[pair]) {
            [newPolymer appendString:rules[pair]];
        }
    }
    [newPolymer appendFormat:@"%C", [polymer characterAtIndex:polymer.length - 1]];
    return newPolymer;
}

NSDictionary *countElements(NSString *polymer) {
    NSMutableDictionary *counts = [NSMutableDictionary dictionary];
    for (NSUInteger i = 0; i < polymer.length; i++) {
        unichar c = [polymer characterAtIndex:i];
        counts[@(c)] = @([counts[@(c)] intValue] + 1);
    }
    return counts;
}

NSArray *minMax(NSDictionary *counts) {
    NSInteger min = NSIntegerMax, max = NSIntegerMin;
    for (NSNumber *count in counts.allValues) {
        if (count.integerValue < min) min = count.integerValue;
        if (count.integerValue > max) max = count.integerValue;
    }
    return @[@(min), @(max)];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSDictionary *rules = readRules(@"input.txt");
        NSString *polymer = [[NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil] componentsSeparatedByString:@"\n"].firstObject;

        for (int step = 0; step < 10; step++) {
            polymer = applyInsertion(polymer, rules);
        }

        NSDictionary *counts = countElements(polymer);
        NSArray *minMaxValues = minMax(counts);
        NSLog(@"%ld", (long)([minMaxValues[1] integerValue] - [minMaxValues[0] integerValue]));
    }
    return 0;
}