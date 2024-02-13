#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *boardingPasses = [input componentsSeparatedByString:@"\n"];
        
        int highestSeatID = 0;
        
        for (NSString *boardingPass in boardingPasses) {
            NSString *rowString = [boardingPass substringToIndex:7];
            NSString *columnString = [boardingPass substringFromIndex:7];
            
            int row = 0;
            for (int i = 0; i < 7; i++) {
                if ([rowString characterAtIndex:i] == 'B') {
                    row += 1 << (6 - i);
                }
            }
            
            int column = 0;
            for (int i = 0; i < 3; i++) {
                if ([columnString characterAtIndex:i] == 'R') {
                    column += 1 << (2 - i);
                }
            }
            
            int seatID = row * 8 + column;
            if (seatID > highestSeatID) {
                highestSeatID = seatID;
            }
        }
        
        printf("%d\n", highestSeatID);
    }
    return 0;
}