#import <Foundation/Foundation.h>
int main() {
    @autoreleasepool {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        NSString *path = @"input.txt";
        if (![fileManager fileExistsAtPath:path]) {
            NSLog(@"File not found");
            return 0;
        }
        
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error.localizedDescription);
            return 0;
        }
        
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"node-x\\d+-y\\d+\\s+\\d+T\\s+(\\d+)T\\s+(\\d+)T\\s+\\d+%" options:0 error:nil];
        
        NSMutableArray<NSNumber *> *usedArray = [NSMutableArray array];
        NSMutableArray<NSNumber *> *availArray = [NSMutableArray array];
        
        for (NSString *line in lines) {
            NSTextCheckingResult *match = [regex firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
            if (match) {
                NSString *usedString = [line substringWithRange:[match rangeAtIndex:1]];
                NSString *availString = [line substringWithRange:[match rangeAtIndex:2]];
                
                [usedArray addObject:@(usedString.intValue)];
                [availArray addObject:@(availString.intValue)];
            }
        }
        
        int count = 0;
        for (int i = 0; i < usedArray.count; i++) {
            for (int j = 0; j < availArray.count; j++) {
                if (i != j && usedArray[i].intValue > 0 && usedArray[i].intValue <= availArray[j].intValue) {
                    count++;
                }
            }
        }
        
        printf("%d\n", count);
    }
    return 0;
}