#import <Foundation/Foundation.h>

typedef NS_ENUM(NSInteger, HandRank) {
    HighCard = 1,
    OnePair,
    TwoPair,
    ThreeKind,
    FullHouse,
    FourKind,
    FiveKind
};

@interface Hand : NSObject
@property (nonatomic, strong) NSString *cards;
@property (nonatomic, assign) NSInteger bid;
@end

@implementation Hand
@end

@interface RankedHand : NSObject
@property (nonatomic, strong) Hand *hand;
@property (nonatomic, assign) NSInteger rank;
@end

@implementation RankedHand
@end

NSMutableArray<NSMutableArray<Hand *> *> *matches;

void findMatches(NSArray<Hand *> *hands) {
    for (Hand *hand in hands) {
        NSMutableDictionary<NSString *, NSNumber *> *count = [NSMutableDictionary dictionary];
        for (NSUInteger i = 0; i < hand.cards.length; i++) {
            NSString *card = [NSString stringWithFormat:@"%C", [hand.cards characterAtIndex:i]];
            count[card] = @(count[card].integerValue + 1);
        }

        NSInteger value = 1;
        for (NSNumber *c in count.allValues) {
            value *= c.integerValue;
        }

        if (value == 1) [matches[6] addObject:hand];
        else if (value == 2) [matches[5] addObject:hand];
        else if (value == 3) [matches[3] addObject:hand];
        else if (value == 4) {
            if (count.count == 2) [matches[1] addObject:hand];
            else [matches[4] addObject:hand];
        } else if (value == 5) [matches[0] addObject:hand];
        else if (value == 6) [matches[2] addObject:hand];
    }
}

NSArray<RankedHand *> *convertAndOrderMatches() {
    NSMutableArray<RankedHand *> *convertedMatches = [NSMutableArray array];

    for (NSArray<Hand *> *category in matches) {
        NSMutableArray<RankedHand *> *temp = [NSMutableArray array];
        for (Hand *hand in category) {
            NSString *cards = [hand.cards stringByReplacingOccurrencesOfString:@"A" withString:@"E"];
            cards = [cards stringByReplacingOccurrencesOfString:@"T" withString:@"A"];
            cards = [cards stringByReplacingOccurrencesOfString:@"J" withString:@"B"];
            cards = [cards stringByReplacingOccurrencesOfString:@"Q" withString:@"C"];
            cards = [cards stringByReplacingOccurrencesOfString:@"K" withString:@"D"];

            NSInteger num = strtol(cards.UTF8String, NULL, 16);
            RankedHand *rankedHand = [[RankedHand alloc] init];
            rankedHand.hand = hand;
            rankedHand.rank = num;
            [temp addObject:rankedHand];
        }

        [temp sortUsingComparator:^NSComparisonResult(RankedHand *obj1, RankedHand *obj2) {
            return obj1.rank < obj2.rank;
        }];

        [convertedMatches addObjectsFromArray:temp];
    }

    return convertedMatches;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        matches = [NSMutableArray arrayWithObjects:[NSMutableArray array], [NSMutableArray array], [NSMutableArray array], [NSMutableArray array], [NSMutableArray array], [NSMutableArray array], [NSMutableArray array], nil];

        NSString *filePath = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [content componentsSeparatedByString:@"\n"];
        NSMutableArray<Hand *> *hands = [NSMutableArray array];

        NSRegularExpression *re = [NSRegularExpression regularExpressionWithPattern:@"[\\dAKQJT]+" options:0 error:nil];
        NSRegularExpression *bidRe = [NSRegularExpression regularExpressionWithPattern:@" [\\d]+" options:0 error:nil];

        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSTextCheckingResult *cardMatch = [re firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];
            NSTextCheckingResult *bidMatch = [bidRe firstMatchInString:line options:0 range:NSMakeRange(0, line.length)];

            NSString *cards = [line substringWithRange:cardMatch.range];
            NSInteger bid = [[line substringWithRange:NSMakeRange(bidMatch.range.location + 1, bidMatch.range.length - 1)] integerValue];

            Hand *hand = [[Hand alloc] init];
            hand.cards = cards;
            hand.bid = bid;
            [hands addObject:hand];
        }

        findMatches(hands);
        NSArray<RankedHand *> *convertedMatches = convertAndOrderMatches();

        NSInteger total = 0;
        for (NSInteger i = 0; i < convertedMatches.count; i++) {
            total += convertedMatches[i].hand.bid * (convertedMatches.count - i);
        }

        NSLog(@"%ld", (long)total);
    }
    return 0;
}