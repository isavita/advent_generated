#import <Foundation/Foundation.h>

int main() {
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

    NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
    int target = [content intValue];

    NSMutableDictionary *grid = [NSMutableDictionary dictionary];
    grid[[NSValue valueWithPoint:NSMakePoint(0, 0)]] = @1;

    int x = 0, y = 0;
    int dx = 0, dy = -1;

    while (YES) {
        if (x == y || (x < 0 && x == -y) || (x > 0 && x == 1 - y)) {
            int temp = dx;
            dx = -dy;
            dy = temp;
        }

        x += dx;
        y += dy;

        int value = 0;
        for (int i = -1; i <= 1; i++) {
            for (int j = -1; j <= 1; j++) {
                NSValue *pointValue = [NSValue valueWithPoint:NSMakePoint(x + i, y + j)];
                value += [grid[pointValue] intValue];
            }
        }
        grid[[NSValue valueWithPoint:NSMakePoint(x, y)]] = @(value);

        if (value > target) {
            printf("%d\n", value);
            break;
        }
    }

    [pool drain];
    return 0;
}