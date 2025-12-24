
#import <Foundation/Foundation.h>

@interface Rule : NSObject
@property (nonatomic, strong) NSMutableArray<NSArray<NSNumber *> *> *options;
@property (nonatomic, strong) NSMutableSet<NSString *> *resolved;
@end
@implementation Rule
@end

NSMutableDictionary<NSNumber *, Rule *> *rules;
NSMutableArray<NSString *> *messages;
NSMutableDictionary<NSNumber *, NSSet<NSString *> *> *cache;

NSSet<NSString *> *resolveRule(NSNumber *num) {
    if (cache[num]) return cache[num];
    Rule *r = rules[num];
    NSMutableSet *res = [NSMutableSet set];
    if (r.options.count == 0) {
        // terminal already in resolved set
        res = r.resolved;
    } else {
        for (NSArray<NSNumber *> *opt in r.options) {
            NSMutableSet *cur = [NSMutableSet setWithObject:@""];
            for (NSNumber *subNum in opt) {
                NSSet *subSet = resolveRule(subNum);
                NSMutableSet *next = [NSMutableSet set];
                for (NSString *a in cur) {
                    for (NSString *b in subSet) {
                        [next addObject:[a stringByAppendingString:b]];
                    }
                }
                cur = next;
            }
            [res unionSet:cur];
        }
    }
    cache[num] = res;
    return res;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray *parts = [input componentsSeparatedByString:@"\n\n"];
        if (parts.count != 2) return 1;
        NSArray *ruleLines = [parts[0] componentsSeparatedByString:@"\n"];
        NSArray *msgLines = [parts[1] componentsSeparatedByString:@"\n"];
        rules = [NSMutableDictionary dictionary];
        messages = [NSMutableArray array];
        for (NSString *line in ruleLines) {
            if (line.length == 0) continue;
            NSArray *kv = [line componentsSeparatedByString:@": "];
            NSNumber *num = @([kv[0] integerValue]);
            Rule *r = [[Rule alloc] init];
            r.options = [NSMutableArray array];
            r.resolved = [NSMutableSet set];
            NSString *rhs = kv[1];
            if ([rhs containsString:@"\""]) {
                NSString *ch = [rhs substringWithRange:NSMakeRange(1, 1)];
                [r.resolved addObject:ch];
            } else {
                NSArray *opts = [rhs componentsSeparatedByString:@" | "];
                for (NSString *opt in opts) {
                    NSArray *parts = [opt componentsSeparatedByString:@" "];
                    NSMutableArray *arr = [NSMutableArray array];
                    for (NSString *p in parts) {
                        if (p.length) [arr addObject:@([p integerValue])];
                    }
                    [r.options addObject:arr];
                }
            }
            rules[num] = r;
        }
        for (NSString *line in msgLines) {
            if (line.length) [messages addObject:line];
        }
        cache = [NSMutableDictionary dictionary];
        NSSet *set42 = resolveRule(@42);
        NSSet *set31 = resolveRule(@31);
        NSArray *arr42 = [[set42 allObjects] sortedArrayUsingSelector:@selector(compare:)];
        NSArray *arr31 = [[set31 allObjects] sortedArrayUsingSelector:@selector(compare:)];
        NSUInteger chunkLen = ((NSString *)arr42.firstObject).length;
        NSUInteger count = 0;
        for (NSString *msg in messages) {
            NSUInteger len = msg.length;
            if (len % chunkLen) continue;
            NSUInteger chunks = len / chunkLen;
            if (chunks < 3) continue;
            NSUInteger i = 0, c42 = 0, c31 = 0;
            while (i < chunks) {
                NSString *sub = [msg substringWithRange:NSMakeRange(i*chunkLen, chunkLen)];
                if (![set42 containsObject:sub]) break;
                i++; c42++;
            }
            while (i < chunks) {
                NSString *sub = [msg substringWithRange:NSMakeRange(i*chunkLen, chunkLen)];
                if (![set31 containsObject:sub]) break;
                i++; c31++;
            }
            if (i == chunks && c31 >= 1 && c42 > c31) count++;
        }
        printf("%lu\n", (unsigned long)count);
    }
    return 0;
}
