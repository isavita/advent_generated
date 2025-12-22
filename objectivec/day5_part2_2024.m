
#import <Foundation/Foundation.h>

@interface Rule : NSObject
@property (nonatomic) NSInteger x;
@property (nonatomic) NSInteger y;
@end
@implementation Rule @end

BOOL isCorrect(NSArray<NSNumber *> *pages, NSArray<Rule *> *rules) {
    NSMutableDictionary *pos = [NSMutableDictionary dictionary];
    [pages enumerateObjectsUsingBlock:^(NSNumber *obj, NSUInteger idx, BOOL *stop) {
        pos[obj] = @(idx);
    }];
    for (Rule *r in rules) {
        NSNumber *px = pos[@(r.x)];
        NSNumber *py = pos[@(r.y)];
        if (px && py && [px integerValue] > [py integerValue]) return NO;
    }
    return YES;
}

NSArray<NSNumber *> *correctOrder(NSArray<NSNumber *> *pages, NSArray<Rule *> *rules) {
    NSMutableDictionary *indeg = [NSMutableDictionary dictionary];
    NSMutableDictionary *adj   = [NSMutableDictionary dictionary];
    for (NSNumber *p in pages) {
        indeg[p] = @0;
        adj[p] = [NSMutableArray array];
    }
    for (Rule *r in rules) {
        NSNumber *x = @(r.x), *y = @(r.y);
        if (indeg[x] && indeg[y]) {
            [adj[x] addObject:y];
            indeg[y] = @([indeg[y] integerValue] + 1);
        }
    }
    NSMutableArray *queue = [NSMutableArray array];
    for (NSNumber *p in pages) if ([indeg[p] integerValue] == 0) [queue addObject:p];
    NSMutableArray *sorted = [NSMutableArray array];
    while (queue.count) {
        NSNumber *u = queue.firstObject;
        [queue removeObjectAtIndex:0];
        [sorted addObject:u];
        for (NSNumber *v in adj[u]) {
            NSInteger d = [indeg[v] integerValue] - 1;
            indeg[v] = @(d);
            if (d == 0) [queue addObject:v];
        }
    }
    return sorted;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 1;
        NSArray *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray<Rule *> *rules = [NSMutableArray array];
        NSMutableArray<NSArray<NSNumber *> *> *updates = [NSMutableArray array];
        BOOL readingRules = YES;
        for (NSString *raw in lines) {
            NSString *line = [raw stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if (line.length == 0) continue;
            if ([line containsString:@"|"]) {
                NSInteger x, y;
                if (sscanf(line.UTF8String, "%ld|%ld", &x, &y) == 2) {
                    Rule *r = [Rule new];
                    r.x = x; r.y = y;
                    [rules addObject:r];
                }
                continue;
            }
            readingRules = NO;
            NSArray *tokens = [line componentsSeparatedByString:@","];
            NSMutableArray<NSNumber *> *pages = [NSMutableArray array];
            for (NSString *t in tokens) {
                NSInteger v = t.integerValue;
                [pages addObject:@(v)];
            }
            if (pages.count) [updates addObject:pages];
        }
        long long total = 0;
        for (NSArray<NSNumber *> *pages in updates) {
            if (!isCorrect(pages, rules)) {
                NSArray<NSNumber *> *fixed = correctOrder(pages, rules);
                if (fixed.count) total += [fixed[fixed.count/2] longLongValue];
            }
        }
        printf("%lld\n", total);
    }
    return 0;
}
