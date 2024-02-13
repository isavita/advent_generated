#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        int horizontalPosition = 0;
        int depth = 0;
        int aim = 0;
        
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                continue;
            }
            
            NSArray *components = [line componentsSeparatedByString:@" "];
            NSString *direction = components[0];
            int value = [components[1] intValue];
            
            if ([direction isEqualToString:@"forward"]) {
                horizontalPosition += value;
                depth += aim * value;
            } else if ([direction isEqualToString:@"down"]) {
                aim += value;
            } else if ([direction isEqualToString:@"up"]) {
                aim -= value;
            }
        }
        
        printf("%d\n", horizontalPosition * depth);
    }
    
    return 0;
}