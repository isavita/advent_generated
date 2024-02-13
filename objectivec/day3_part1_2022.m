#import <Foundation/Foundation.h>

int itemPriority(unichar item) {
    if (item >= 'a' && item <= 'z') {
        return (int)(item - 'a' + 1);
    }
    return (int)(item - 'A') + 27;
}

int main() {
    @autoreleasepool {
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error opening file: %@", error);
            return 1;
        }

        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        int sum = 0;
        for (NSString *line in lines) {
            NSUInteger half = [line length] / 2;
            NSString *firstCompartment = [line substringToIndex:half];
            NSString *secondCompartment = [line substringFromIndex:half];

            NSMutableDictionary<NSNumber *, NSNumber *> *compartmentMap = [NSMutableDictionary dictionary];
            for (int i = 0; i < [firstCompartment length]; i++) {
                unichar item = [firstCompartment characterAtIndex:i];
                NSNumber *count = compartmentMap[@(item)];
                if (count) {
                    compartmentMap[@(item)] = @(count.intValue + 1);
                } else {
                    compartmentMap[@(item)] = @1;
                }
            }

            for (int i = 0; i < [secondCompartment length]; i++) {
                unichar item = [secondCompartment characterAtIndex:i];
                if (compartmentMap[@(item)]) {
                    sum += itemPriority(item);
                    break;
                }
            }
        }

        printf("%d\n", sum);
    }
    return 0;
}