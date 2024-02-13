#import <Foundation/Foundation.h>

@interface Ship : NSObject

@property int x;
@property int y;
@property int facing;

- (void) processInstruction:(unichar)action value:(int)value;

@end

@implementation Ship

- (void) processInstruction:(unichar)action value:(int)value {
    switch (action) {
        case 'N':
            self.y += value;
            break;
        case 'S':
            self.y -= value;
            break;
        case 'E':
            self.x += value;
            break;
        case 'W':
            self.x -= value;
            break;
        case 'L':
            self.facing = (self.facing - value + 360) % 360;
            break;
        case 'R':
            self.facing = (self.facing + value) % 360;
            break;
        case 'F':
            switch (self.facing) {
                case 0:
                    self.x += value;
                    break;
                case 90:
                    self.y -= value;
                    break;
                case 180:
                    self.x -= value;
                    break;
                case 270:
                    self.y += value;
                    break;
            }
            break;
    }
}

@end

int main() {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        
        Ship *ship = [[Ship alloc] init];
        ship.x = 0;
        ship.y = 0;
        ship.facing = 0;
        
        for (NSString *line in lines) {
            if ([line length] == 0) {
                continue;
            }
            unichar action = [line characterAtIndex:0];
            int value = [[line substringFromIndex:1] intValue];
            [ship processInstruction:action value:value];
        }
        
        int manhattanDistance = abs(ship.x) + abs(ship.y);
        printf("%d\n", manhattanDistance);
    }
    return 0;
}

int abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}