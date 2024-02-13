#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        int steps = 0;
        int row = 0;
        int col = (int)[lines[0] rangeOfString:@"|"].location;
        char direction = 'd';
        
        while (row >= 0 && row < lines.count && col >= 0 && col < [lines[row] length]) {
            char currentChar = [lines[row] characterAtIndex:col];
            if (currentChar == ' ') {
                break;
            } else if (currentChar == '|' || currentChar == '-') {
                // Continue in the same direction
            } else if (currentChar == '+') {
                if (direction == 'd' || direction == 'u') {
                    if (col - 1 >= 0 && [lines[row] characterAtIndex:col - 1] != ' ') {
                        direction = 'l';
                    } else {
                        direction = 'r';
                    }
                } else {
                    if (row - 1 >= 0 && [lines[row - 1] characterAtIndex:col] != ' ') {
                        direction = 'u';
                    } else {
                        direction = 'd';
                    }
                }
            } else if (currentChar >= 'A' && currentChar <= 'Z') {
                printf("%c", currentChar);
            }
            
            if (direction == 'd') {
                row++;
            } else if (direction == 'u') {
                row--;
            } else if (direction == 'l') {
                col--;
            } else if (direction == 'r') {
                col++;
            }
            
            steps++;
        }
        
        printf("\n%d\n", steps);
    }
    return 0;
}