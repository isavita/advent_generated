#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *boardingPasses = [input componentsSeparatedByString:@"\n"];
        
        int highestSeatID = 0;
        NSMutableArray *seatIDs = [NSMutableArray array];
        
        for (NSString *boardingPass in boardingPasses) {
            NSString *rowString = [boardingPass substringToIndex:7];
            NSString *columnString = [boardingPass substringFromIndex:7];
            
            int row = 0;
            int column = 0;
            
            for (int i = 0; i < rowString.length; i++) {
                if ([rowString characterAtIndex:i] == 'B') {
                    row += 1 << (rowString.length - 1 - i);
                }
            }
            
            for (int i = 0; i < columnString.length; i++) {
                if ([columnString characterAtIndex:i] == 'R') {
                    column += 1 << (columnString.length - 1 - i);
                }
            }
            
            int seatID = row * 8 + column;
            [seatIDs addObject:@(seatID)];
            
            if (seatID > highestSeatID) {
                highestSeatID = seatID;
            }
        }
        
        printf("Part One: %d\n", highestSeatID);
        
        [seatIDs sortUsingSelector:@selector(compare:)];
        
        int mySeatID = 0;
        
        for (int i = 1; i < seatIDs.count - 1; i++) {
            int currentSeatID = [seatIDs[i] intValue];
            int nextSeatID = [seatIDs[i + 1] intValue];
            
            if (nextSeatID - currentSeatID == 2) {
                mySeatID = currentSeatID + 1;
                break;
            }
        }
        
        printf("Part Two: %d\n", mySeatID);
    }
    return 0;
}