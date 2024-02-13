#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error;
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        int count = 0;
        for (NSString *line in lines) {
            NSArray *parts = [line componentsSeparatedByString:@" | "];
            NSString *output = parts[1];
            NSArray *digits = [output componentsSeparatedByString:@" "];
            for (NSString *digit in digits) {
                if ([digit length] == 2 || [digit length] == 4 || [digit length] == 3 || [digit length] == 7) {
                    count++;
                }
            }
        }
        
        printf("%d\n", count);
    }
    return 0;
}