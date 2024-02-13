#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        int targetSquare = [input intValue];
        
        int x = 0, y = 0;
        int dx = 0, dy = -1;
        int steps = 0;
        
        for (int i = 1; i < targetSquare; i++) {
            if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1-y)) {
                int temp = dx;
                dx = -dy;
                dy = temp;
            }
            x += dx;
            y += dy;
            steps++;
        }
        
        printf("%d\n", abs(x) + abs(y));
    }
    return 0;
}