
#import <Foundation/Foundation.h>

@interface Node : NSObject
@property (strong) NSString *name;
@property (strong) NSMutableArray<NSNumber *> *nbr;
@end
@implementation Node
@end

static NSMutableArray<Node *> *nodes;

static NSInteger idx(NSString *s) {
    for (NSInteger i = 0; i < nodes.count; ++i)
        if ([nodes[i].name isEqualToString:s]) return i;
    Node *n = [Node new];
    n.name = s;
    n.nbr = [NSMutableArray array];
    [nodes addObject:n];
    return nodes.count - 1;
}

static void add_edge(NSInteger u, NSInteger v) {
    [nodes[u].nbr addObject:@(v)];
}

static long long dfs(NSInteger cur, NSInteger tgt, long long *memo) {
    if (cur == tgt) return 1;
    if (memo[cur] != -1) return memo[cur];
    long long sum = 0;
    for (NSNumber *n in nodes[cur].nbr)
        sum += dfs(n.integerValue, tgt, memo);
    return memo[cur] = sum;
}

static long long count_paths(NSInteger s, NSInteger t) {
    long long *memo = calloc(nodes.count, sizeof(long long));
    for (NSInteger i = 0; i < nodes.count; ++i) memo[i] = -1;
    long long res = dfs(s, t, memo);
    free(memo);
    return res;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        nodes = [NSMutableArray array];
        NSString *txt = [NSString stringWithContentsOfFile:@"input.txt"
                                                  encoding:NSUTF8StringEncoding
                                                     error:nil];
        for (NSString *line in [txt componentsSeparatedByCharactersInSet:
                              [NSCharacterSet newlineCharacterSet]]) {
            NSString *s = [line stringByTrimmingCharactersInSet:
                           [NSCharacterSet whitespaceCharacterSet]];
            if (!s.length) continue;
            NSArray *parts = [s componentsSeparatedByString:@":"];
            if (parts.count != 2) continue;
            NSString *src = [parts[0] stringByTrimmingCharactersInSet:
                             [NSCharacterSet whitespaceCharacterSet]];
            NSInteger u = idx(src);
            for (NSString *tok in [parts[1] componentsSeparatedByCharactersInSet:
                                   [NSCharacterSet whitespaceCharacterSet]]) {
                if (!tok.length) continue;
                NSInteger v = idx(tok);
                add_edge(u, v);
            }
        }
        NSInteger svr = idx(@"svr"), dac = idx(@"dac"),
                  fft = idx(@"fft"), out = idx(@"out");
        long long s1 = count_paths(svr, dac) * count_paths(dac, fft) * count_paths(fft, out);
        long long s2 = count_paths(svr, fft) * count_paths(fft, dac) * count_paths(dac, out);
        printf("Paths (svr->dac->fft->out): %lld\n", s1);
        printf("Paths (svr->fft->dac->out): %lld\n", s2);
        printf("Total paths visiting both: %lld\n", s1 + s2);
    }
    return 0;
}
