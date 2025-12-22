#import <Foundation/Foundation.h>

@interface Wire : NSObject
@property (strong, nonatomic) NSString *name;
@property (assign) NSInteger value;
@property (assign) BOOL initialized;
@end
@implementation Wire
@end

@interface Gate : NSObject
@property (strong, nonatomic) NSString *input1;
@property (strong, nonatomic) NSString *input2;
@property (strong, nonatomic) NSString *output;
@property (strong, nonatomic) NSString *operation;
@end
@implementation Gate
@end

static Wire *findWire(NSMutableArray<Wire *> *wires, NSString *name) {
    for (Wire *w in wires) if ([w.name isEqualToString:name]) return w;
    return nil;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *text = [NSString stringWithContentsOfFile:@"input.txt"
                                                     encoding:NSUTF8StringEncoding
                                                        error:&error];
        if (!text) { NSLog(@"%@", error); return 1; }
        NSArray *lines = [text componentsSeparatedByString:@"\n"];

        NSMutableArray<Wire *> *wires = [NSMutableArray array];
        NSMutableArray<Gate *>  *gates  = [NSMutableArray array];

        for (NSString *line in lines) {
            if ([line containsString:@"->"]) break;
            NSArray *parts = [line componentsSeparatedByString:@":"];
            if (parts.count != 2) continue;
            Wire *w = [Wire new];
            w.name = [[parts[0] stringByTrimmingCharactersInSet:
                       NSCharacterSet.whitespaceCharacterSet] copy];
            w.value = [parts[1] integerValue];
            w.initialized = YES;
            [wires addObject:w];
        }

        for (NSString *line in lines) {
            if (![line containsString:@"->"]) continue;
            // e.g. "x00 AND y00 -> z00"
            NSArray *sides = [line componentsSeparatedByString:@" -> "];
            if (sides.count != 2) continue;
            NSArray *lhs = [sides[0] componentsSeparatedByString:@" "];
            if (lhs.count != 3) continue;
            Gate *g = [Gate new];
            g.input1   = lhs[0];
            g.operation= lhs[1];
            g.input2   = lhs[2];
            g.output   = sides[1];
            [gates addObject:g];
        }

        BOOL changed = YES;
        while (changed) {
            changed = NO;
            for (Gate *g in gates) {
                Wire *in1 = findWire(wires, g.input1);
                Wire *in2 = findWire(wires, g.input2);
                Wire *out = findWire(wires, g.output);
                if (!in1)  { in1  = [Wire new]; in1.name  = g.input1;  [wires addObject:in1]; }
                if (!in2)  { in2  = [Wire new]; in2.name  = g.input2;  [wires addObject:in2]; }
                if (!out)  { out  = [Wire new]; out.name  = g.output;  [wires addObject:out]; }

                if (in1.initialized && in2.initialized && !out.initialized) {
                    if ([g.operation isEqualToString:@"AND"]) out.value = in1.value & in2.value;
                    else if ([g.operation isEqualToString:@"OR" ]) out.value = in1.value | in2.value;
                    else if ([g.operation isEqualToString:@"XOR"]) out.value = in1.value ^ in2.value;
                    out.initialized = YES;
                    changed = YES;
                }
            }
        }

        long long outNum = 0;
        for (Wire *w in wires) {
            if ([w.name hasPrefix:@"z"] && w.initialized && w.value) {
                int bit = [[w.name substringFromIndex:1] intValue];
                outNum |= 1LL << bit;
            }
        }
        printf("%lld\n", outNum);
    }
    return 0;
}