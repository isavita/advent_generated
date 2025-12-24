#import <Foundation/Foundation.h>

#define MAX_MONKEYS 10
#define ROUNDS 10000

typedef struct {
    NSMutableArray *items;
    char op;
    long long opVal;
    long long div;
    int next[2];
    long long inspections;
} Monkey;

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *data = [NSString stringWithContentsOfFile:@"input.txt"
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        NSArray *lines = [data componentsSeparatedByString:@"\n"];
        Monkey monkeys[MAX_MONKEYS];
        int numMonkeys = 0;
        long long totalDiv = 1;

        for (int i = 0; i < lines.count; ) {
            if (numMonkeys >= MAX_MONKEYS) break;
            Monkey *m = &monkeys[numMonkeys];
            m->items = [NSMutableArray array];
            m->inspections = 0;

            // items
            i++;
            NSString *itemLine = lines[i];
            NSArray *parts = [[itemLine substringFromIndex:[itemLine rangeOfString:@":"].location+2]
                            componentsSeparatedByString:@", "];
            for (NSString *p in parts)
                [m->items addObject:@([p longLongValue])];

            // op
            i++;
            NSString *opLine = lines[i];
            NSArray *opParts = [opLine componentsSeparatedByString:@" "];
            m->op = [opParts[opParts.count-2] characterAtIndex:0];
            m->opVal = [opParts.lastObject isEqualToString:@"old"] ? -1 : [opParts.lastObject longLongValue];

            // div
            i++;
            m->div = [[lines[i] componentsSeparatedByString:@" "].lastObject longLongValue];
            totalDiv *= m->div;

            // true
            i++;
            m->next[1] = [[lines[i] componentsSeparatedByString:@" "].lastObject intValue];
            // false
            i++;
            m->next[0] = [[lines[i] componentsSeparatedByString:@" "].lastObject intValue];

            numMonkeys++;
            i++;
            while (i < lines.count && [lines[i] length] == 0) i++;
        }

        for (int round = 0; round < ROUNDS; round++) {
            for (int idx = 0; idx < numMonkeys; idx++) {
                Monkey *m = &monkeys[idx];
                while ([m->items count]) {
                    m->inspections++;
                    long long item = [[m->items objectAtIndex:0] longLongValue];
                    [m->items removeObjectAtIndex:0];
                    long long operand = (m->opVal == -1) ? item : m->opVal;
                    if (m->op == '+') item += operand;
                    else item *= operand;
                    item %= totalDiv;
                    int target = m->next[(item % m->div == 0)];
                    [monkeys[target].items addObject:@(item)];
                }
            }
        }

        long long top[2] = {0,0};
        for (int i = 0; i < numMonkeys; i++) {
            long long v = monkeys[i].inspections;
            if (v > top[0]) { top[1] = top[0]; top[0] = v; }
            else if (v > top[1]) top[1] = v;
        }
        printf("%lld\n", top[0] * top[1]);
    }
    return 0;
}