
#import <Foundation/Foundation.h>

typedef NS_ENUM(NSInteger, ModuleType) {
    BROADCASTER,
    FLIP_FLOP,
    CONJUNCTION
};

@interface Module : NSObject
@property (nonatomic, strong) NSString *name;
@property (nonatomic) ModuleType type;
@property (nonatomic, strong) NSArray<NSString *> *connectsTo;
- (instancetype)initWithName:(NSString *)n type:(ModuleType)t connectsTo:(NSArray<NSString *> *)ct;
@end

@implementation Module
- (instancetype)initWithName:(NSString *)n type:(ModuleType)t connectsTo:(NSArray<NSString *> *)ct {
    self = [super init];
    if (self) {
        _name = n;
        _type = t;
        _connectsTo = ct;
    }
    return self;
}
@end

@interface FlipFlop : Module
@property (nonatomic) BOOL state;
- (instancetype)initWithName:(NSString *)n connectsTo:(NSArray<NSString *> *)ct;
@end

@implementation FlipFlop
- (instancetype)initWithName:(NSString *)n connectsTo:(NSArray<NSString *> *)ct {
    self = [super initWithName:n type:FLIP_FLOP connectsTo:ct];
    if (self) {
        _state = NO;
    }
    return self;
}
@end

@interface Conjunction : Module
@property (nonatomic, strong) NSMutableDictionary<NSString *, NSNumber *> *watches;
- (instancetype)initWithName:(NSString *)n connectsTo:(NSArray<NSString *> *)ct;
@end

@implementation Conjunction
- (instancetype)initWithName:(NSString *)n connectsTo:(NSArray<NSString *> *)ct {
    self = [super initWithName:n type:CONJUNCTION connectsTo:ct];
    if (self) {
        _watches = [NSMutableDictionary dictionary];
    }
    return self;
}
@end

@interface Broadcaster : Module
- (instancetype)initWithName:(NSString *)n connectsTo:(NSArray<NSString *> *)ct;
@end

@implementation Broadcaster
- (instancetype)initWithName:(NSString *)n connectsTo:(NSArray<NSString *> *)ct {
    return [super initWithName:n type:BROADCASTER connectsTo:ct];
}
@end

@interface State : NSObject
@property (nonatomic, strong) NSString *from;
@property (nonatomic, strong) NSString *to;
@property (nonatomic) BOOL pulse;
- (instancetype)initWithFrom:(NSString *)f to:(NSString *)t pulse:(BOOL)p;
@end

@implementation State
- (instancetype)initWithFrom:(NSString *)f to:(NSString *)t pulse:(BOOL)p {
    self = [super init];
    if (self) {
        _from = f;
        _to = t;
        _pulse = p;
    }
    return self;
}
@end

static Module *parseLine(NSString *line) {
    NSArray *parts = [line componentsSeparatedByString:@" -> "];
    NSString *left = parts[0];
    NSString *right = parts[1];
    NSArray *connectsTo = [right componentsSeparatedByString:@", "];

    if ([left isEqualToString:@"broadcaster"]) {
        return [[Broadcaster alloc] initWithName:left connectsTo:connectsTo];
    } else if ([left hasPrefix:@"%"]) {
        return [[FlipFlop alloc] initWithName:[left substringFromIndex:1] connectsTo:connectsTo];
    } else {
        return [[Conjunction alloc] initWithName:[left substringFromIndex:1] connectsTo:connectsTo];
    }
}

static void completeWatches(NSMutableDictionary<NSString *, Module *> *connections) {
    for (Module *module in connections.allValues) {
        if (module.type == CONJUNCTION) {
            Conjunction *conj = (Conjunction *)module;
            for (Module *m2 in connections.allValues) {
                for (NSString *dest in m2.connectsTo) {
                    if ([dest isEqualToString:conj.name]) {
                        conj.watches[m2.name] = @NO;
                    }
                }
            }
        }
    }
}

static BOOL simulatePress(NSMutableDictionary<NSString *, Module *> *connections,
                          NSMutableDictionary<NSString *, NSNumber *> *loopLengths,
                          long long pressNumber) {

    NSMutableArray<State *> *q = [NSMutableArray array];
    [q addObject:[[State alloc] initWithFrom:@"button" to:@"broadcaster" pulse:NO]];

    BOOL rxLow = NO;

    while (q.count) {
        State *s = q.firstObject;
        [q removeObjectAtIndex:0];

        Module *m = connections[s.to];
        if (!m) {
            if ([s.to isEqualToString:@"rx"] && !s.pulse) rxLow = YES;
            continue;
        }

        BOOL incoming = s.pulse;

        switch (m.type) {
            case BROADCASTER: {
                for (NSString *next in m.connectsTo) {
                    [q addObject:[[State alloc] initWithFrom:m.name to:next pulse:incoming]];
                }
                break;
            }
            case FLIP_FLOP: {
                FlipFlop *ff = (FlipFlop *)m;
                if (!incoming) {
                    ff.state = !ff.state;
                    for (NSString *next in ff.connectsTo) {
                        [q addObject:[[State alloc] initWithFrom:ff.name to:next pulse:ff.state]];
                    }
                }
                break;
            }
            case CONJUNCTION: {
                Conjunction *conj = (Conjunction *)m;
                conj.watches[s.from] = @(incoming);

                BOOL allHigh = YES;
                for (NSNumber *v in conj.watches.allValues) {
                    if (!v.boolValue) {
                        allHigh = NO;
                        break;
                    }
                }
                BOOL outgoing = !allHigh;

                if (loopLengths[conj.name] && outgoing && [loopLengths[conj.name] isEqualToNumber:@-1]) {
                    loopLengths[conj.name] = @(pressNumber);
                }

                for (NSString *next in conj.connectsTo) {
                    [q addObject:[[State alloc] initWithFrom:conj.name to:next pulse:outgoing]];
                }
                break;
            }
        }
    }
    return rxLow;
}

static long long gcd(long long a, long long b) {
    return b == 0 ? a : gcd(b, a % b);
}

static long long lcm(long long a, long long b) {
    if (a == 0 || b == 0) return 0;
    return llabs(a * b) / gcd(a, b);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        if (!content) {
            NSLog(@"Error reading input.txt");
            return 1;
        }

        NSMutableDictionary<NSString *, Module *> *connections = [NSMutableDictionary dictionary];
        for (NSString *line in [content componentsSeparatedByString:@"\n"]) {
            if (line.length == 0) continue;
            Module *m = parseLine(line);
            connections[m.name] = m;
        }

        completeWatches(connections);

        NSString *rxFeeder = nil;
        for (Module *m in connections.allValues) {
            for (NSString *dest in m.connectsTo) {
                if ([dest isEqualToString:@"rx"]) {
                    rxFeeder = m.name;
                    break;
                }
            }
            if (rxFeeder) break;
        }

        Conjunction *rxFeederConj = (Conjunction *)connections[rxFeeder];
        NSMutableDictionary<NSString *, NSNumber *> *loopLengths = [NSMutableDictionary dictionary];
        for (NSString *key in rxFeederConj.watches) {
            loopLengths[key] = @-1;
        }

        long long pressNumber = 0;
        while (YES) {
            pressNumber++;
            simulatePress(connections, loopLengths, pressNumber);

            BOOL allFound = YES;
            for (NSNumber *v in loopLengths.allValues) {
                if ([v isEqualToNumber:@-1]) {
                    allFound = NO;
                    break;
                }
            }
            if (allFound) break;
        }

        long long result = 1;
        for (NSNumber *v in loopLengths.allValues) {
            result = lcm(result, v.longLongValue);
        }

        printf("%lld\n", result);
    }
    return 0;
}
