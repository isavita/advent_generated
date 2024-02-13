#import <Foundation/Foundation.h>

int countBags(NSString *color, NSDictionary<NSString *, NSArray<NSDictionary<NSString *, NSNumber *> *> *> *rules) {
    int count = 1;
    for (NSDictionary<NSString *, NSNumber *> *rule in rules[color]) {
        count += [rule[@"Count"] intValue] * countBags(rule[@"Color"], rules);
    }
    return count;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSMutableDictionary<NSString *, NSMutableArray<NSDictionary<NSString *, NSNumber *> *> *> *rules = [NSMutableDictionary dictionary];
        NSRegularExpression *ruleRegex = [NSRegularExpression regularExpressionWithPattern:@"(\\d+) (\\w+ \\w+) bags?[,.]" options:0 error:nil];
        
        for (NSString *line in lines) {
            NSArray<NSTextCheckingResult *> *matches = [ruleRegex matchesInString:line options:0 range:NSMakeRange(0, [line length])];
            NSArray<NSString *> *parts = [line componentsSeparatedByString:@" bags contain "];
            NSString *container = parts[0];
            NSString *contents = parts[1];
            
            if ([contents isEqualToString:@"no other bags."]) {
                continue;
            }
            
            for (NSTextCheckingResult *match in matches) {
                NSNumberFormatter *formatter = [[NSNumberFormatter alloc] init];
                formatter.numberStyle = NSNumberFormatterDecimalStyle;
                NSNumber *count = [formatter numberFromString:[line substringWithRange:[match rangeAtIndex:1]]];
                NSString *color = [line substringWithRange:[match rangeAtIndex:2]];
                NSDictionary<NSString *, NSNumber *> *bagRule = @{@"Color": color, @"Count": count};
                
                if (rules[container] == nil) {
                    rules[container] = [NSMutableArray array];
                }
                [rules[container] addObject:bagRule];
            }
        }
        
        int totalBags = countBags(@"shiny gold", rules) - 1;
        printf("%d\n", totalBags);
    }
    return 0;
}