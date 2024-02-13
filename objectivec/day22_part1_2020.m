#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *decks = [input componentsSeparatedByString:@"\n\n"];
        
        NSArray *player1 = [[decks objectAtIndex:0] componentsSeparatedByString:@"\n"];
        NSArray *player2 = [[decks objectAtIndex:1] componentsSeparatedByString:@"\n"];
        
        NSMutableArray *deck1 = [NSMutableArray arrayWithArray:[player1 subarrayWithRange:NSMakeRange(1, player1.count - 1)]];
        NSMutableArray *deck2 = [NSMutableArray arrayWithArray:[player2 subarrayWithRange:NSMakeRange(1, player2.count - 1)]];
        
        while (deck1.count > 0 && deck2.count > 0) {
            int card1 = [[deck1 firstObject] intValue];
            int card2 = [[deck2 firstObject] intValue];
            
            [deck1 removeObjectAtIndex:0];
            [deck2 removeObjectAtIndex:0];
            
            if (card1 > card2) {
                [deck1 addObject:@(card1)];
                [deck1 addObject:@(card2)];
            } else {
                [deck2 addObject:@(card2)];
                [deck2 addObject:@(card1)];
            }
        }
        
        long score = 0;
        NSArray *winner = deck1.count > 0 ? deck1 : deck2;
        for (int i = 0; i < winner.count; i++) {
            score += [winner[i] intValue] * (winner.count - i);
        }
        
        printf("%ld\n", score);
    }
    return 0;
}