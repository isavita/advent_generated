
#import <Foundation/Foundation.h>

static long long calculateScore(NSArray *deck) {
    long long score = 0;
    NSUInteger count = deck.count;
    for (NSUInteger i = 0; i < count; ++i) {
        score += [deck[i] longLongValue] * (long long)(count - i);
    }
    return score;
}

static BOOL playRecursiveCombat(NSMutableArray *p1, NSMutableArray *p2) {
    NSMutableSet *previous = [NSMutableSet set];
    while (p1.count && p2.count) {
        NSString *key = [[p1 componentsJoinedByString:@","] stringByAppendingFormat:@"|%@", [p2 componentsJoinedByString:@","]];
        if ([previous containsObject:key]) return YES;
        [previous addObject:key];

        NSInteger card1 = [p1[0] integerValue];
        NSInteger card2 = [p2[0] integerValue];
        [p1 removeObjectAtIndex:0];
        [p2 removeObjectAtIndex:0];

        BOOL p1Wins;
        if (p1.count >= card1 && p2.count >= card2) {
            NSMutableArray *sub1 = [NSMutableArray arrayWithArray:[p1 subarrayWithRange:NSMakeRange(0, card1)]];
            NSMutableArray *sub2 = [NSMutableArray arrayWithArray:[p2 subarrayWithRange:NSMakeRange(0, card2)]];
            p1Wins = playRecursiveCombat(sub1, sub2);
        } else {
            p1Wins = card1 > card2;
        }

        if (p1Wins) {
            [p1 addObject:@(card1)];
            [p1 addObject:@(card2)];
        } else {
            [p2 addObject:@(card2)];
            [p2 addObject:@(card1)];
        }
    }
    return p1.count > 0;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];

        NSMutableArray *deck1 = [NSMutableArray array];
        NSMutableArray *deck2 = [NSMutableArray array];
        NSMutableArray *current = deck1;

        for (NSString *raw in lines) {
            NSString *line = [raw stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            if (line.length == 0) { current = deck2; continue; }
            if ([line hasPrefix:@"Player"]) continue;
            NSInteger card = [line integerValue];
            [current addObject:@(card)];
        }

        BOOL p1Won = playRecursiveCombat(deck1, deck2);
        NSArray *winner = p1Won ? deck1 : deck2;
        printf("%lld\n", calculateScore(winner));
    }
    return 0;
}
