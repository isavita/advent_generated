#import <Foundation/Foundation.h>

int itemPriority(unichar item) {
    if (item >= 'a' && item <= 'z') {
        return (int)(item-'a') + 1;
    }
    return (int)(item-'A') + 27;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error opening file: %@", error);
            return 1;
        }
        
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        int sum = 0;
        int groupLineCounter = 0;
        NSMutableDictionary *groupItems[3];
        
        for (NSString *line in lines) {
            NSMutableDictionary *itemsMap = [NSMutableDictionary dictionary];
            for (int i = 0; i < [line length]; i++) {
                unichar item = [line characterAtIndex:i];
                NSNumber *count = itemsMap[@(item)];
                if (count) {
                    itemsMap[@(item)] = @([count intValue] + 1);
                } else {
                    itemsMap[@(item)] = @1;
                }
            }
            groupItems[groupLineCounter] = itemsMap;
            groupLineCounter++;
            
            if (groupLineCounter == 3) {
                NSMutableDictionary *commonItems = [NSMutableDictionary dictionary];
                for (NSNumber *item in groupItems[0]) {
                    if ([groupItems[1][item] intValue] > 0 && [groupItems[2][item] intValue] > 0) {
                        commonItems[item] = @1;
                    }
                }
                for (NSNumber *item in commonItems) {
                    sum += itemPriority([item unsignedShortValue]);
                    break; // Since we need only one common item per group
                }
                groupLineCounter = 0;
            }
        }
        
        printf("%d\n", sum);
    }
    return 0;
}