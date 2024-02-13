#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        NSString *filePath = @"input.txt";
        NSString *input = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        
        int steps = [input intValue];
        
        NSMutableArray *buffer = [NSMutableArray arrayWithObject:@0];
        int currentPosition = 0;
        
        for (int i = 1; i <= 2017; i++) {
            currentPosition = (currentPosition + steps) % buffer.count + 1;
            [buffer insertObject:@(i) atIndex:currentPosition];
        }
        
        printf("Part One: %d\n", [buffer[(currentPosition + 1) % buffer.count] intValue]);
        
        int valueAfterZero = 0;
        currentPosition = 0;
        
        for (int i = 1; i <= 50000000; i++) {
            currentPosition = (currentPosition + steps) % i + 1;
            if (currentPosition == 1) {
                valueAfterZero = i;
            }
        }
        
        printf("Part Two: %d\n", valueAfterZero);
    }
    return 0;
}