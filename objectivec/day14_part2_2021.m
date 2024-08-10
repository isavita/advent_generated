#import <Foundation/Foundation.h>

NSDictionary *readInput(NSString *filename, NSString **template) {
    NSString *input = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:nil];
    NSArray *lines = [input componentsSeparatedByString:@"\n"];
    *template = lines[0];
    NSMutableDictionary *rules = [NSMutableDictionary dictionary];
    
    for (NSInteger i = 2; i < lines.count; i++) {
        NSString *line = lines[i];
        if (line.length > 0) {
            NSArray *parts = [line componentsSeparatedByString:@" -> "];
            rules[parts[0]] = parts[1];
        }
    }
    return rules;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *template;
        NSDictionary *rules = readInput(@"input.txt", &template);
        NSMutableDictionary *pairCounts = [NSMutableDictionary dictionary];
        
        for (NSInteger i = 0; i < template.length - 1; i++) {
            NSString *pair = [template substringWithRange:NSMakeRange(i, 2)];
            pairCounts[pair] = @([pairCounts[pair] longLongValue] + 1);
        }

        for (NSInteger step = 0; step < 40; step++) {
            NSMutableDictionary *newPairCounts = [NSMutableDictionary dictionary];
            for (NSString *pair in pairCounts) {
                long long count = [pairCounts[pair] longLongValue];
                NSString *insert = rules[pair];
                if (insert) {
                    NSString *left = [NSString stringWithFormat:@"%c%@", [pair characterAtIndex:0], insert];
                    NSString *right = [NSString stringWithFormat:@"%@%c", insert, [pair characterAtIndex:1]];
                    newPairCounts[left] = @([newPairCounts[left] longLongValue] + count);
                    newPairCounts[right] = @([newPairCounts[right] longLongValue] + count);
                } else {
                    newPairCounts[pair] = @([newPairCounts[pair] longLongValue] + count);
                }
            }
            pairCounts = newPairCounts;
        }

        NSMutableDictionary *elementCounts = [NSMutableDictionary dictionary];
        for (NSString *pair in pairCounts) {
            long long count = [pairCounts[pair] longLongValue];
            elementCounts[@([pair characterAtIndex:0])] = @([elementCounts[@([pair characterAtIndex:0])] longLongValue] + count);
        }
        elementCounts[@([template characterAtIndex:template.length - 1])] = @([elementCounts[@([template characterAtIndex:template.length - 1])] longLongValue] + 1);

        long long maxCount = 0, minCount = LLONG_MAX;
        for (NSNumber *count in elementCounts.allValues) {
            long long value = [count longLongValue];
            if (value > maxCount) maxCount = value;
            if (value < minCount) minCount = value;
        }

        NSLog(@"%lld", maxCount - minCount);
    }
    return 0;
}