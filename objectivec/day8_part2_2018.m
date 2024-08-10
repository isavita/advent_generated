#import <Foundation/Foundation.h>

NSArray *readInput(NSString *filename) {
    NSString *content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:nil];
    NSArray *parts = [content componentsSeparatedByString:@" "];
    NSMutableArray *numbers = [NSMutableArray arrayWithCapacity:parts.count];
    for (NSString *part in parts) {
        [numbers addObject:@([part integerValue])];
    }
    return numbers;
}

NSInteger parseTree(NSArray *data, NSInteger *index) {
    NSInteger childCount = [data[*index] integerValue];
    NSInteger metaCount = [data[*index + 1] integerValue];
    *index += 2;

    NSMutableArray *childValues = [NSMutableArray arrayWithCapacity:childCount];
    for (NSInteger i = 0; i < childCount; i++) {
        [childValues addObject:@(parseTree(data, index))];
    }

    NSInteger value = 0;
    if (childCount == 0) {
        for (NSInteger i = 0; i < metaCount; i++) {
            value += [data[*index + i] integerValue];
        }
    } else {
        for (NSInteger i = 0; i < metaCount; i++) {
            NSInteger metadata = [data[*index + i] integerValue];
            if (metadata > 0 && metadata <= childCount) {
                value += [childValues[metadata - 1] integerValue];
            }
        }
    }
    *index += metaCount;

    return value;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray *numbers = readInput(@"input.txt");
        NSInteger index = 0;
        NSInteger value = parseTree(numbers, &index);
        NSLog(@"%ld", (long)value);
    }
    return 0;
}