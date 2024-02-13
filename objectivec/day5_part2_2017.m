#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *fileContents = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        NSMutableArray *jumpOffsets = [NSMutableArray array];
        for (NSString *line in lines) {
            if (![line isEqualToString:@""]) {
                [jumpOffsets addObject:@(line.intValue)];
            }
        }
        
        int steps = 0;
        int currentPosition = 0;
        
        while (currentPosition >= 0 && currentPosition < jumpOffsets.count) {
            int offset = [jumpOffsets[currentPosition] intValue];
            
            if (offset >= 3) {
                jumpOffsets[currentPosition] = @(offset - 1);
            } else {
                jumpOffsets[currentPosition] = @(offset + 1);
            }
            
            currentPosition += offset;
            steps++;
        }
        
        printf("%d\n", steps);
    }
    return 0;
}