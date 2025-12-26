
#import <Foundation/Foundation.h>

@interface Monkey : NSObject
@property (nonatomic, strong) NSMutableArray<NSNumber *> *items;
@property (nonatomic) char op;                // '+' or '*'
@property (nonatomic) long long operand;      // -1 means "old"
@property (nonatomic) int testDivisible;
@property (nonatomic) int ifTrue;
@property (nonatomic) int ifFalse;
@property (nonatomic) long long inspected;
- (long long)applyOperation:(long long)value;
@end

@implementation Monkey
- (instancetype)init {
    if (self = [super init]) {
        _items = [NSMutableArray array];
        _inspected = 0;
    }
    return self;
}
- (long long)applyOperation:(long long)value {
    if (_op == '+') return value + _operand;
    if (_operand == -1) return value * value;
    return value * _operand;
}
@end

static Monkey *parseMonkey(NSArray<NSString *> *lines) {
    Monkey *m = [[Monkey alloc] init];

    // items
    NSString *itemsPart = [[lines[1] componentsSeparatedByString:@": "] lastObject];
    for (NSString *num in [itemsPart componentsSeparatedByString:@", "]) {
        [m.items addObject:@(num.longLongValue)];
    }

    // operation
    NSString *expr = [[lines[2] componentsSeparatedByString:@"= "] lastObject];
    NSArray *tokens = [expr componentsSeparatedByString:@" "]; // old, op, right
    m.op = [tokens[1] characterAtIndex:0];
    NSString *right = tokens[2];
    m.operand = [right isEqualToString:@"old"] ? -1 : right.longLongValue;

    // test divisor
    m.testDivisible = [[lines[3] componentsSeparatedByString:@"by "] lastObject].intValue;

    // destinations
    m.ifTrue = [[lines[4] componentsSeparatedByString:@"monkey "] lastObject].intValue;
    m.ifFalse = [[lines[5] componentsSeparatedByString:@"monkey "] lastObject].intValue;

    return m;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *rawLines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray<Monkey *> *monkeys = [NSMutableArray array];

        for (NSUInteger i = 0; i + 5 < rawLines.count; ) {
            if (![rawLines[i] hasPrefix:@"Monkey"]) { i++; continue; }
            NSArray *block = @[
                rawLines[i],
                rawLines[i+1],
                rawLines[i+2],
                rawLines[i+3],
                rawLines[i+4],
                rawLines[i+5]
            ];
            [monkeys addObject:parseMonkey(block)];
            i += 7; // skip blank line after each block
        }

        for (int round = 0; round < 20; ++round) {
            for (Monkey *m in monkeys) {
                while (m.items.count) {
                    m.inspected++;
                    long long item = m.items[0].longLongValue;
                    [m.items removeObjectAtIndex:0];
                    item = [m applyOperation:item];
                    item /= 3;
                    Monkey *target = (item % m.testDivisible == 0) ? monkeys[m.ifTrue] : monkeys[m.ifFalse];
                    [target.items addObject:@(item)];
                }
            }
        }

        NSMutableArray<NSNumber *> *counts = [NSMutableArray array];
        for (Monkey *m in monkeys) [counts addObject:@(m.inspected)];
        [counts sortUsingComparator:^NSComparisonResult(NSNumber *a, NSNumber *b) {
            return [b compare:a];
        }];

        long long result = counts[0].longLongValue * counts[1].longLongValue;
        printf("%lld\n", result);
    }
    return 0;
}
