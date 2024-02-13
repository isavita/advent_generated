#import <Foundation/Foundation.h>

int main() {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        int width = [(NSString *)lines[0] length];
        int height = (int)[lines count];
        
        int trees = 0;
        int x = 0;
        for (int y = 0; y < height; y++) {
            if ([lines[y] characterAtIndex:x % width] == '#') {
                trees++;
            }
            x += 3;
        }
        
        printf("%d\n", trees);
    }
    return 0;
}