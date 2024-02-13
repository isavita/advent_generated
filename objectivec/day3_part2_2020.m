#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        int width = [lines[0] length];
        int height = [lines count];
        
        int slopes[5][2] = {{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}};
        int treeCounts[5] = {0};
        
        for (int i = 0; i < 5; i++) {
            int right = slopes[i][0];
            int down = slopes[i][1];
            int x = 0;
            int y = 0;
            
            while (y < height) {
                if ([lines[y] characterAtIndex:x % width] == '#') {
                    treeCounts[i]++;
                }
                x += right;
                y += down;
            }
        }
        
        int result = 1;
        for (int i = 0; i < 5; i++) {
            result *= treeCounts[i];
        }
        
        printf("%d\n", result);
    }
    
    return 0;
}