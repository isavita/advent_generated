
#import <Foundation/Foundation.h>

@interface Node : NSObject
@property (nonatomic, copy) NSString *name;
@property (nonatomic) NSInteger weight;
@property (nonatomic, strong) NSMutableArray<NSString *> *holds;
@end

@implementation Node
- (instancetype)initWithName:(NSString *)n weight:(NSInteger)w {
    if (self = [super init]) {
        _name = [n copy];
        _weight = w;
        _holds = [NSMutableArray array];
    }
    return self;
}
@end

static NSMutableDictionary<NSString *, Node *> *nodes;
static BOOL printed = NO;

static NSInteger dfs(NSString *name, BOOL *balanced) {
    Node *node = nodes[name];
    NSInteger total = node.weight;
    NSMutableDictionary<NSNumber *, NSNumber *> *weightCount = [NSMutableDictionary dictionary];
    NSMutableDictionary<NSNumber *, NSMutableArray<NSString *> *> *weightToChild = [NSMutableDictionary dictionary];
    for (NSString *childName in node.holds) {
        BOOL childBal = YES;
        NSInteger w = dfs(childName, &childBal);
        if (!childBal) {
            *balanced = NO;
            return 0;
        }
        total += w;
        NSNumber *key = @(w);
        weightCount[key] = @([weightCount[key] integerValue] + 1);
        if (!weightToChild[key]) weightToChild[key] = [NSMutableArray array];
        [weightToChild[key] addObject:childName];
    }
    if (weightCount.count > 1 && !printed) {
        NSNumber *correctWeight = nil, *incorrectWeight = nil;
        for (NSNumber *w in weightCount) {
            NSInteger cnt = [weightCount[w] integerValue];
            if (cnt == 1) incorrectWeight = w;
            else correctWeight = w;
        }
        if (incorrectWeight && correctWeight) {
            NSString *badChild = weightToChild[incorrectWeight].firstObject;
            Node *badNode = nodes[badChild];
            NSInteger diff = [correctWeight integerValue] - [incorrectWeight integerValue];
            printf("%ld\n", (long)(badNode.weight + diff));
            printed = YES;
        }
        *balanced = NO;
        return 0;
    }
    *balanced = YES;
    return total;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        nodes = [NSMutableDictionary dictionary];
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSArray<NSString *> *parts = [line componentsSeparatedByString:@" "];
            NSString *name = parts[0];
            NSInteger weight = [[parts[1] substringWithRange:NSMakeRange(1, parts[1].length-2)] integerValue];
            nodes[name] = [[Node alloc] initWithName:name weight:weight];
        }
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            NSArray<NSString *> *parts = [line componentsSeparatedByString:@" "];
            NSString *name = parts[0];
            Node *node = nodes[name];
            if (parts.count <= 3) continue;
            for (NSInteger i = 3; i < parts.count; i++) {
                NSString *token = parts[i];
                if ([token hasPrefix:@"-"]) continue;
                NSString *child = [token stringByTrimmingCharactersInSet:[NSCharacterSet characterSetWithCharactersInString:@",\n"]];
                if (child.length) [node.holds addObject:child];
            }
        }
        BOOL bal = YES;
        dfs(@"dtacyn", &bal);
    }
    return 0;
}
