
#import <Foundation/Foundation.h>

@interface Hand : NSObject
@property (nonatomic, copy) NSString *cards;
@property (nonatomic) NSInteger bid;
+ (instancetype)handWithCards:(NSString *)c bid:(NSInteger)b;
@end

@implementation Hand
+ (instancetype)handWithCards:(NSString *)c bid:(NSInteger)b {
    Hand *h = [Hand new];
    h.cards = c;
    h.bid = b;
    return h;
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt"
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        NSArray<NSString *> *lines = [input componentsSeparatedByString:@"\n"];
        NSMutableArray<Hand *> *hands = [NSMutableArray array];
        NSCharacterSet *cardSet = [NSCharacterSet characterSetWithCharactersInString:@"23456789TJQKA"];
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSRange r = [line rangeOfCharacterFromSet:cardSet];
            NSString *cards = r.location != NSNotFound ? [line substringWithRange:NSMakeRange(r.location, 5)] : @"";
            NSInteger bid = [[line substringFromIndex:r.location+5] integerValue];
            [hands addObject:[Hand handWithCards:cards bid:bid]];
        }

        NSMutableArray<NSMutableArray<Hand *> *> *matches = [NSMutableArray arrayWithCapacity:7];
        for (int i=0;i<7;i++) [matches addObject:[NSMutableArray array]];

        NSDictionary *valueDict = @{@"J":@1,@"2":@2,@"3":@3,@"4":@4,@"5":@5,@"6":@6,@"7":@7,@"8":@8,@"9":@9,@"T":@10,@"Q":@11,@"K":@12,@"A":@13};

        for (Hand *h in hands) {
            NSMutableDictionary<NSString *,NSNumber *> *cnt = [NSMutableDictionary dictionary];
            for (NSUInteger i=0;i<h.cards.length;i++) {
                NSString *ch = [h.cards substringWithRange:NSMakeRange(i,1)];
                cnt[ch] = @([cnt[ch] integerValue] + 1);
            }
            if (cnt[@"J"] && [cnt[@"J"] integerValue] > 0) {
                NSString *highKey = @"J";
                NSInteger highV = 0;
                for (NSString *k in cnt) {
                    if ([k isEqualToString:@"J"]) continue;
                    NSInteger v = [cnt[k] integerValue];
                    if (v > highV || (v == highV && [valueDict[k] integerValue] > [valueDict[highKey] integerValue])) {
                        highKey = k; highV = v;
                    }
                }
                if (![highKey isEqualToString:@"J"]) {
                    cnt[highKey] = @([cnt[highKey] integerValue] + [cnt[@"J"] integerValue]);
                    [cnt removeObjectForKey:@"J"];
                }
            }
            NSInteger mult = 1;
            for (NSNumber *n in cnt.allValues) mult *= n.integerValue;
            switch (mult) {
                case 1:  [matches[6] addObject:h]; break;
                case 2:  [matches[5] addObject:h]; break;
                case 3:  [matches[3] addObject:h]; break;
                case 4:  (cnt.count==2 ? [matches[1] addObject:h] : [matches[4] addObject:h]); break;
                case 5:  [matches[0] addObject:h]; break;
                case 6:  [matches[2] addObject:h]; break;
                default: break;
            }
        }

        NSMutableArray<NSArray<NSNumber *> *> *ordered = [NSMutableArray array];
        for (NSMutableArray<Hand *> *group in matches) {
            NSMutableArray<NSArray<NSNumber *> *> *temp = [NSMutableArray array];
            for (Hand *h in group) {
                NSMutableString *s = [h.cards mutableCopy];
                [s replaceOccurrencesOfString:@"A" withString:@"E" options:0 range:NSMakeRange(0,s.length)];
                [s replaceOccurrencesOfString:@"T" withString:@"A" options:0 range:NSMakeRange(0,s.length)];
                [s replaceOccurrencesOfString:@"J" withString:@"1" options:0 range:NSMakeRange(0,s.length)];
                [s replaceOccurrencesOfString:@"Q" withString:@"C" options:0 range:NSMakeRange(0,s.length)];
                [s replaceOccurrencesOfString:@"K" withString:@"D" options:0 range:NSMakeRange(0,s.length)];
                unsigned long long val = strtoull(s.UTF8String, NULL, 16);
                [temp addObject:@[@(val), @(h.bid)]];
            }
            [temp sortUsingComparator:^NSComparisonResult(NSArray *a, NSArray *b) {
                return [b[0] compare:a[0]];
            }];
            [ordered addObjectsFromArray:temp];
        }

        long long total = 0;
        NSUInteger n = ordered.count;
        for (NSUInteger i=0;i<n;i++) total += [ordered[i][1] longLongValue] * (n - i);
        printf("%lld\n", total);
    }
    return 0;
}
