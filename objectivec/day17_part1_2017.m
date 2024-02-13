#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }
        
        int step = [input intValue];
        NSMutableArray *buffer = [NSMutableArray arrayWithObject:@0];
        int currentPosition = 0;
        
        for (int i = 1; i <= 2017; i++) {
            currentPosition = (currentPosition + step) % buffer.count + 1;
            [buffer insertObject:@(i) atIndex:currentPosition];
        }
        
        printf("%d\n", [buffer[(currentPosition + 1) % buffer.count] intValue]);
    }
    return 0;
}