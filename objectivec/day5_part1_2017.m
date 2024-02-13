#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableArray *jumpOffsets = [NSMutableArray array];
        for (NSString *line in lines) {
            [jumpOffsets addObject:@(line.intValue)];
        }
        
        int steps = 0;
        int index = 0;
        
        while (index >= 0 && index < jumpOffsets.count) {
            int offset = [jumpOffsets[index] intValue];
            jumpOffsets[index] = @(offset + 1);
            index += offset;
            steps++;
        }
        
        printf("%d\n", steps);
    }
    return 0;
}