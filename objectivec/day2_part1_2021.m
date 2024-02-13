#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        int horizontalPosition = 0;
        int depth = 0;
        
        for (NSString *line in lines) {
            NSArray *components = [line componentsSeparatedByString:@" "];
            NSString *direction = components[0];
            int value = [components[1] intValue];
            
            if ([direction isEqualToString:@"forward"]) {
                horizontalPosition += value;
            } else if ([direction isEqualToString:@"down"]) {
                depth += value;
            } else if ([direction isEqualToString:@"up"]) {
                depth -= value;
            }
        }
        
        printf("%d\n", horizontalPosition * depth);
    }
    return 0;
}