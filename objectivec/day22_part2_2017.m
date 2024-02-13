#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *grid = [NSMutableDictionary dictionary];
        NSInteger startX, startY;
        for (NSInteger y = 0; y < lines.count; y++) {
            NSString *line = lines[y];
            for (NSInteger x = 0; x < line.length; x++) {
                unichar c = [line characterAtIndex:x];
                if (c == '#') {
                    grid[[NSString stringWithFormat:@"%ld,%ld", x, y]] = @(2);
                }
            }
            startX = line.length/2;
            startY = y/2;
        }
        
        NSArray *dx = @[@(0), @(1), @(0), @(-1)];
        NSArray *dy = @[@(-1), @(0), @(1), @(0)];
        
        NSInteger x = startX, y = startY, dir = 0;
        NSInteger infectedCount = 0;
        
        for (NSInteger i = 0; i < 10000000; i++) {
            NSString *pos = [NSString stringWithFormat:@"%ld,%ld", x, y];
            NSInteger current = [grid[pos] integerValue];
            switch (current) {
                case 0:
                    dir = (dir - 1 + 4) % 4;
                    grid[pos] = @(1);
                    break;
                case 1:
                    grid[pos] = @(2);
                    infectedCount++;
                    break;
                case 2:
                    dir = (dir + 1) % 4;
                    grid[pos] = @(3);
                    break;
                case 3:
                    dir = (dir + 2) % 4;
                    grid[pos] = @(0);
                    break;
                default:
                    break;
            }
            x += [dx[dir] integerValue];
            y += [dy[dir] integerValue];
        }
        
        printf("%ld\n", infectedCount);
    }
    return 0;
}