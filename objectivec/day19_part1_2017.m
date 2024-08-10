#import <Foundation/Foundation.h>

int main(int argc, char *argv[]) {
    NSError *error;
    NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    if (!input) {
        NSLog(@"%@", error);
        return 1;
    }

    NSArray *grid = [input componentsSeparatedByString:@"\n"];
    NSMutableArray *letters = [NSMutableArray array];

    NSInteger x = 0, y = 0;
    for (NSInteger i = 0; i < [grid[0] length]; i++) {
        if ([grid[0] characterAtIndex:i] == '|') {
            x = i;
            break;
        }
    }

    NSInteger dx = 0, dy = 1;

    while (true) {
        if (x < 0 || x >= [grid[0] length] || y < 0 || y >= [grid count]) {
            break;
        }

        unichar cell = [grid[y] characterAtIndex:x];

        if (cell == ' ') {
            break;
        }

        if (cell >= 'A' && cell <= 'Z') {
            [letters addObject:[NSString stringWithFormat:@"%C", cell]];
        }

        if (cell == '+') {
            if (dx == 0) {
                if (x > 0 && ([grid[y] characterAtIndex:x-1] == '-' || ([grid[y] characterAtIndex:x-1] >= 'A' && [grid[y] characterAtIndex:x-1] <= 'Z'))) {
                    dx = -1; dy = 0;
                } else {
                    dx = 1; dy = 0;
                }
            } else {
                if (y > 0 && ([grid[y-1] characterAtIndex:x] == '|' || ([grid[y-1] characterAtIndex:x] >= 'A' && [grid[y-1] characterAtIndex:x] <= 'Z'))) {
                    dx = 0; dy = -1;
                } else {
                    dx = 0; dy = 1;
                }
            }
        }

        x += dx;
        y += dy;
    }

    NSLog(@"%@", [letters componentsJoinedByString:@""]);
    return 0;
}