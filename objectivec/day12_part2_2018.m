#import <Foundation/Foundation.h>

@interface Pot : NSObject
@property (nonatomic) NSInteger index;
@property (nonatomic) char plant;
- (instancetype)initWithIndex:(NSInteger)idx plant:(char)p;
@end
@implementation Pot
- (instancetype)initWithIndex:(NSInteger)idx plant:(char)p {
    self = [super init];
    if (self) { self.index = idx; self.plant = p; }
    return self;
}
@end

@interface State : NSObject
@property (nonatomic) NSMutableArray<Pot *> *pots;
- (void)addPot:(NSInteger)idx plant:(char)p;
- (char)plantAt:(NSInteger)idx;
- (NSInteger)minIndex;
- (NSInteger)maxIndex;
- (long long)sum;
- (NSString *)pattern;
@end
@implementation State
- (instancetype)init {
    self = [super init];
    if (self) { _pots = [NSMutableArray array]; }
    return self;
}
- (void)addPot:(NSInteger)idx plant:(char)p {
    if (p == '#') [self.pots addObject:[[Pot alloc] initWithIndex:idx plant:p]];
}
- (char)plantAt:(NSInteger)idx {
    for (Pot *pot in self.pots) if (pot.index == idx) return pot.plant;
    return '.';
}
- (NSInteger)minIndex {
    if (self.pots.count == 0) return 0;
    NSInteger m = self.pots[0].index;
    for (Pot *pot in self.pots) if (pot.index < m) m = pot.index;
    return m;
}
- (NSInteger)maxIndex {
    if (self.pots.count == 0) return 0;
    NSInteger m = self.pots[0].index;
    for (Pot *pot in self.pots) if (pot.index > m) m = pot.index;
    return m;
}
- (long long)sum {
    long long s = 0;
    for (Pot *pot in self.pots) if (pot.plant == '#') s += pot.index;
    return s;
}
- (NSString *)pattern {
    NSInteger mn = [self minIndex], mx = [self maxIndex];
    NSMutableString *str = [NSMutableString stringWithCapacity:mx - mn + 1];
    for (NSInteger i = mn; i <= mx; i++) [str appendFormat:@"%c", [self plantAt:i]];
    return str;
}
@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        NSArray *lines = [data componentsSeparatedByString:@"\n"];
        NSString *initial = nil;
        NSMutableDictionary<NSString *, NSString *> *rules = [NSMutableDictionary dictionary];
        for (NSString *line in lines) {
            if ([line containsString:@"initial state:"])
                initial = [[[line componentsSeparatedByString:@": "] lastObject] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
            else if ([line containsString:@"=>"]) {
                NSArray *parts = [line componentsSeparatedByString:@" => "];
                if (parts.count == 2)
                    rules[[parts[0] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet]] =
                        [[parts[1] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet] substringToIndex:1];
            }
        }

        State *state = [[State alloc] init];
        for (NSInteger i = 0; i < initial.length; i++)
            if ([initial characterAtIndex:i] == '#') [state addPot:i plant:'#'];

        NSString *prevPattern = nil;
        long long prevSum = 0, offset = 0;
        for (long long gen = 0; gen < 50000000000; gen++) {
            State *next = [[State alloc] init];
            NSInteger mn = [state minIndex], mx = [state maxIndex];
            for (NSInteger i = mn - 2; i <= mx + 2; i++) {
                NSMutableString *pat = [NSMutableString stringWithCapacity:5];
                for (NSInteger j = i - 2; j <= i + 2; j++) [pat appendFormat:@"%c", [state plantAt:j]];
                NSString *res = rules[pat];
                if ([res isEqualToString:@"#"]) [next addPot:i plant:'#'];
            }
            state = next;

            NSString *curPattern = [state pattern];
            long long curSum = [state sum];
            if (prevPattern && [curPattern isEqualToString:prevPattern]) {
                offset = curSum - prevSum;
                long long rem = 50000000000 - gen - 1;
                printf("%lld\n", curSum + offset * rem);
                return 0;
            }
            prevPattern = curPattern;
            prevSum = curSum;
        }
        printf("%lld\n", [state sum]);
    }
    return 0;
}