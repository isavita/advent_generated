#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        int currentButton = 5;
        NSString *keypad[3][3] = { {@"1", @"2", @"3"},
                                    {@"4", @"5", @"6"},
                                    {@"7", @"8", @"9"} };
        
        for (NSString *line in lines) {
            for (int i = 0; i < line.length; i++) {
                unichar direction = [line characterAtIndex:i];
                
                int row = (currentButton - 1) / 3;
                int col = (currentButton - 1) % 3;
                
                if (direction == 'U' && row > 0 && ![keypad[row - 1][col] isEqualToString:@""]) {
                    currentButton -= 3;
                } else if (direction == 'D' && row < 2 && ![keypad[row + 1][col] isEqualToString:@""]) {
                    currentButton += 3;
                } else if (direction == 'L' && col > 0 && ![keypad[row][col - 1] isEqualToString:@""]) {
                    currentButton--;
                } else if (direction == 'R' && col < 2 && ![keypad[row][col + 1] isEqualToString:@""]) {
                    currentButton++;
                }
            }
            
            printf("%s", [keypad[(currentButton - 1) / 3][(currentButton - 1) % 3] UTF8String]);
        }
    }
    return 0;
}