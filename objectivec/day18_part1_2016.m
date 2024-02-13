#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *rows = [input componentsSeparatedByString:@"\n"];
        
        NSString *currentRow = rows[0];
        int safeTiles = 0;
        
        for (int i = 0; i < 40; i++) {
            safeTiles += [[currentRow componentsSeparatedByString:@"."] count] - 1;
            
            NSMutableString *nextRow = [NSMutableString string];
            for (int j = 0; j < currentRow.length; j++) {
                char left = (j == 0) ? '.' : [currentRow characterAtIndex:j - 1];
                char center = [currentRow characterAtIndex:j];
                char right = (j == currentRow.length - 1) ? '.' : [currentRow characterAtIndex:j + 1];
                
                if ((left == '^' && center == '^' && right == '.') ||
                    (center == '^' && right == '^' && left == '.') ||
                    (left == '^' && center == '.' && right == '.') ||
                    (right == '^' && center == '.' && left == '.')) {
                    [nextRow appendString:@"^"];
                } else {
                    [nextRow appendString:@"."];
                }
            }
            
            currentRow = nextRow;
        }
        
        printf("%d\n", safeTiles);
    }
    return 0;
}