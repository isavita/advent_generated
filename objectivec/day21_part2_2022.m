
#import <Foundation/Foundation.h>

@interface Monkey : NSObject
@property (nonatomic, copy) NSString *name;
@property (nonatomic) long long val;
@property (nonatomic) BOOL hasVal;
@property (nonatomic, strong) Monkey *left;
@property (nonatomic, strong) Monkey *right;
@property (nonatomic) char op;
@end

@implementation Monkey
@end

static NSMutableDictionary<NSString *, Monkey *> *dict;

static Monkey *M(NSString *n) {
    Monkey *m = dict[n];
    if (!m) {
        m = [Monkey new];
        m.name = n;
        m.hasVal = NO;
        dict[n] = m;
    }
    return m;
}

static BOOL solve(Monkey *m, long long *res) {
    if (m.hasVal) { *res = m.val; return YES; }
    if (m.left && m.right) {
        long long l, r;
        if (solve(m.left, &l) && solve(m.right, &r)) {
            switch (m.op) {
                case '+': *res = l + r; return YES;
                case '-': *res = l - r; return YES;
                case '*': *res = l * r; return YES;
                case '/': if (r) *res = l / r; return r != 0;
                case '=': *res = (l == r); return YES;
            }
        }
    }
    return NO;
}

static long long expect(Monkey *m, long long target) {
    if ([m.name isEqualToString:@"humn"]) return target;
    long long lv, rv;
    BOOL lOk = solve(m.left, &lv);
    BOOL rOk = solve(m.right, &rv);
    if (!lOk) {
        switch (m.op) {
            case '+': return expect(m.left, target - rv);
            case '-': return expect(m.left, target + rv);
            case '*': return expect(m.left, target / rv);
            case '/': return expect(m.left, target * rv);
            case '=': return expect(m.left, rv);
        }
    }
    if (!rOk) {
        switch (m.op) {
            case '+': return expect(m.right, target - lv);
            case '-': return expect(m.right, lv - target);
            case '*': return expect(m.right, target / lv);
            case '/': return expect(m.right, lv / target);
            case '=': return expect(m.right, lv);
        }
    }
    abort();
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        dict = [NSMutableDictionary dictionary];
        NSString *text = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        for (NSString *line in [text componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]]) {
            if (line.length == 0) continue;
            char name[6], left[6], right[6], op;
            long long val;
            if (sscanf(line.UTF8String, "%5[^:]: %lld", name, &val) == 2) {
                Monkey *m = M([NSString stringWithUTF8String:name]);
                m.val = val;
                m.hasVal = YES;
            } else if (sscanf(line.UTF8String, "%5[^:]: %5s %c %5s", name, left, &op, right) == 4) {
                Monkey *m = M([NSString stringWithUTF8String:name]);
                m.left = M([NSString stringWithUTF8String:left]);
                m.right = M([NSString stringWithUTF8String:right]);
                m.op = op;
                m.hasVal = NO;
            }
        }
        Monkey *root = M(@"root");
        Monkey *humn = M(@"humn");
        humn.hasVal = NO;
        root.op = '=';
        long long result = expect(root, 0);
        printf("%lld\n", result);
    }
    return 0;
}
