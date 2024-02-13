#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *seats = [input componentsSeparatedByString:@"\n"];
        
        int numRows = (int)[seats count];
        int numCols = (int)[seats[0] length];
        
        NSMutableArray *currentSeats = [NSMutableArray arrayWithArray:seats];
        NSMutableArray *nextSeats = [NSMutableArray arrayWithArray:seats];
        
        BOOL changed = YES;
        
        while (changed) {
            changed = NO;
            
            for (int i = 0; i < numRows; i++) {
                for (int j = 0; j < numCols; j++) {
                    if ([currentSeats[i] characterAtIndex:j] == 'L') {
                        int occupiedAdjacent = 0;
                        
                        for (int x = -1; x <= 1; x++) {
                            for (int y = -1; y <= 1; y++) {
                                if (x == 0 && y == 0) {
                                    continue;
                                }
                                
                                int newRow = i + x;
                                int newCol = j + y;
                                
                                if (newRow >= 0 && newRow < numRows && newCol >= 0 && newCol < numCols) {
                                    if ([currentSeats[newRow] characterAtIndex:newCol] == '#') {
                                        occupiedAdjacent++;
                                    }
                                }
                            }
                        }
                        
                        if (occupiedAdjacent == 0) {
                            nextSeats[i] = [nextSeats[i] stringByReplacingCharactersInRange:NSMakeRange(j, 1) withString:@"#"];
                            changed = YES;
                        }
                    } else if ([currentSeats[i] characterAtIndex:j] == '#') {
                        int occupiedAdjacent = 0;
                        
                        for (int x = -1; x <= 1; x++) {
                            for (int y = -1; y <= 1; y++) {
                                if (x == 0 && y == 0) {
                                    continue;
                                }
                                
                                int newRow = i + x;
                                int newCol = j + y;
                                
                                if (newRow >= 0 && newRow < numRows && newCol >= 0 && newCol < numCols) {
                                    if ([currentSeats[newRow] characterAtIndex:newCol] == '#') {
                                        occupiedAdjacent++;
                                    }
                                }
                            }
                        }
                        
                        if (occupiedAdjacent >= 4) {
                            nextSeats[i] = [nextSeats[i] stringByReplacingCharactersInRange:NSMakeRange(j, 1) withString:@"L"];
                            changed = YES;
                        }
                    }
                }
            }
            
            currentSeats = [NSMutableArray arrayWithArray:nextSeats];
        }
        
        int occupiedSeats = 0;
        
        for (NSString *row in currentSeats) {
            for (int i = 0; i < [row length]; i++) {
                if ([row characterAtIndex:i] == '#') {
                    occupiedSeats++;
                }
            }
        }
        
        printf("%d\n", occupiedSeats);
    }
    return 0;
}