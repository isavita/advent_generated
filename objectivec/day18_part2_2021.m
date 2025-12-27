
#import <Foundation/Foundation.h>

@interface SnailNumber : NSObject
@property (nonatomic) long long value;
@property (nonatomic, strong) SnailNumber *left;
@property (nonatomic, strong) SnailNumber *right;
- (instancetype)initWithValue:(long long)v left:(SnailNumber *)l right:(SnailNumber *)r;
- (BOOL)isRegular;
- (SnailNumber *)add:(SnailNumber *)other;
- (SnailNumber *)reduce;
- (BOOL)explodeDepth:(int)depth leftVal:(long long *)l rightVal:(long long *)r;
- (void)addLeft:(long long)v;
- (void)addRight:(long long)v;
- (BOOL)split;
- (long long)magnitude;
- (SnailNumber *)deepCopy;
@end

@implementation SnailNumber
- (instancetype)initWithValue:(long long)v left:(SnailNumber *)l right:(SnailNumber *)r {
    if (self = [super init]) {
        _value = v; _left = l; _right = r;
    }
    return self;
}
- (BOOL)isRegular { return _left == nil && _right == nil; }
- (SnailNumber *)add:(SnailNumber *)other {
    SnailNumber *sum = [[SnailNumber alloc] initWithValue:0 left:self right:other];
    return [sum reduce];
}
- (SnailNumber *)reduce {
    while (YES) {
        long long l=0,r=0;
        if ([self explodeDepth:0 leftVal:&l rightVal:&r]) continue;
        if (![self split]) break;
    }
    return self;
}
- (BOOL)explodeDepth:(int)depth leftVal:(long long *)l rightVal:(long long *)r {
    if (self.isRegular) return NO;
    if (depth == 4) {
        *l = self.left.value; *r = self.right.value;
        self.value = 0; self.left = nil; self.right = nil;
        return YES;
    }
    if ([self.left explodeDepth:depth+1 leftVal:l rightVal:r]) {
        if (*r && self.right) [self.right addLeft:*r];
        *r = 0;
        return YES;
    }
    if ([self.right explodeDepth:depth+1 leftVal:l rightVal:r]) {
        if (*l && self.left) [self.left addRight:*l];
        *l = 0;
        return YES;
    }
    return NO;
}
- (void)addLeft:(long long)v {
    if (self.isRegular) self.value += v;
    else [self.left addLeft:v];
}
- (void)addRight:(long long)v {
    if (self.isRegular) self.value += v;
    else [self.right addRight:v];
}
- (BOOL)split {
    if (self.isRegular) {
        if (self.value >= 10) {
            long long half = self.value / 2;
            self.left = [[SnailNumber alloc] initWithValue:half left:nil right:nil];
            self.right = [[SnailNumber alloc] initWithValue:self.value - half left:nil right:nil];
            self.value = 0;
            return YES;
        }
        return NO;
    }
    return [self.left split] || [self.right split];
}
- (long long)magnitude {
    if (self.isRegular) return self.value;
    return 3 * [self.left magnitude] + 2 * [self.right magnitude];
}
- (SnailNumber *)deepCopy {
    if (self.isRegular) return [[SnailNumber alloc] initWithValue:self.value left:nil right:nil];
    return [[SnailNumber alloc] initWithValue:0 left:[self.left deepCopy] right:[self.right deepCopy]];
}
@end

SnailNumber *parse(NSString *s) {
    if (![s hasPrefix:@"["]) return [[SnailNumber alloc] initWithValue:s.longLongValue left:nil right:nil];
    NSInteger balance = 0; NSInteger split = 0;
    for (NSInteger i = 1; i < s.length - 1; ++i) {
        unichar c = [s characterAtIndex:i];
        if (c == '[') ++balance;
        else if (c == ']') --balance;
        else if (c == ',' && balance == 0) { split = i; break; }
    }
    NSString *l = [s substringWithRange:NSMakeRange(1, split - 1)];
    NSString *r = [s substringWithRange:NSMakeRange(split + 1, s.length - split - 2)];
    return [[SnailNumber alloc] initWithValue:0 left:parse(l) right:parse(r)];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *err = nil;
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
        if (!content) { return 1; }
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray<SnailNumber *> *nums = [NSMutableArray array];
        for (NSString *line in lines) {
            if (line.length) [nums addObject:parse(line)];
        }
        long long best = 0;
        for (NSUInteger i = 0; i < nums.count; ++i) {
            for (NSUInteger j = 0; j < nums.count; ++j) if (i != j) {
                SnailNumber *a = [nums[i] deepCopy];
                SnailNumber *b = [nums[j] deepCopy];
                long long m1 = [[a add:b] magnitude];
                a = [nums[i] deepCopy];
                b = [nums[j] deepCopy];
                long long m2 = [[b add:a] magnitude];
                best = MAX(best, MAX(m1, m2));
            }
        }
        printf("%lld\n", best);
    }
    return 0;
}
