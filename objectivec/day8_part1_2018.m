#import <Foundation/Foundation.h>

NSArray *readInput(NSString *filename) {
    NSString *content = [NSString stringWithContentsOfFile:filename encoding:NSUTF8StringEncoding error:nil];
    NSArray *parts = [content componentsSeparatedByString:@" "];
    NSMutableArray *numbers = [NSMutableArray arrayWithCapacity:parts.count];
    for (NSString *part in parts) {
        [numbers addObject:@(part.intValue)];
    }
    return numbers;
}

NSInteger parseTree(NSArray *data, NSInteger *index) {
    NSInteger childCount = [data[*index] integerValue];
    NSInteger metaCount = [data[*index + 1] integerValue];
    *index += 2;

    NSInteger sum = 0;
    for (NSInteger i = 0; i < childCount; i++) {
        sum += parseTree(data, index);
    }

    for (NSInteger i = 0; i < metaCount; i++) {
        sum += [data[*index + i] integerValue];
    }
    *index += metaCount;

    return sum;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSArray *numbers = readInput(@"input.txt");
        NSInteger index = 0;
        NSInteger sum = parseTree(numbers, &index);
        NSLog(@"%ld", (long)sum);
    }
    return 0;
}