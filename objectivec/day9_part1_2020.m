#import <Foundation/Foundation.h>

BOOL isValid(NSInteger number, NSArray<NSNumber *> *previousNumbers) {
    NSMutableSet *seen = [NSMutableSet set];
    for (NSNumber *n in previousNumbers) {
        if ([seen containsObject:@(number - n.integerValue)]) {
            return YES;
        }
        [seen addObject:n];
    }
    return NO;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *error;
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error opening file: %@", error);
            return 1;
        }
        
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray<NSNumber *> *numbers = [NSMutableArray array];
        
        for (NSString *line in lines) {
            if (line.length > 0) {
                [numbers addObject:@(line.integerValue)];
            }
        }
        
        NSInteger preambleLength = 25;
        for (NSInteger i = preambleLength; i < numbers.count; i++) {
            if (!isValid(numbers[i].integerValue, [numbers subarrayWithRange:NSMakeRange(i - preambleLength, preambleLength)])) {
                NSLog(@"%@", numbers[i]);
                break;
            }
        }
    }
    return 0;
}