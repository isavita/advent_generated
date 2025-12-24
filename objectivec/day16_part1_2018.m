
#import <Foundation/Foundation.h>

typedef struct {
    char a, b, action;
    unsigned char matchCount[256];
} OP;

static inline int AValue(OP *op, int regs[], unsigned char ins[]) {
    return (op->a == 'r') ? regs[ins[1]] : ins[1];
}
static inline int BValue(OP *op, int regs[], unsigned char ins[]) {
    return (op->b == 'r') ? regs[ins[2]] : ins[2];
}
static void runOp(OP *op, int regs[], unsigned char ins[], int res[]) {
    int A = AValue(op, regs, ins);
    int B = BValue(op, regs, ins);
    switch (op->action) {
        case '+': res[ins[3]] = A + B; break;
        case '*': res[ins[3]] = A * B; break;
        case '&': res[ins[3]] = A & B; break;
        case '|': res[ins[3]] = A | B; break;
        case 'a': res[ins[3]] = A; break;
        case '>': res[ins[3]] = A > B; break;
        case '=': res[ins[3]] = A == B; break;
    }
}
static int match(int a[], int b[], int n) {
    for (int i = 0; i < n; i++) if (a[i] != b[i]) return 0;
    return 1;
}
static void addMatch(OP *op, unsigned char c) {
    if (strchr((char *)op->matchCount, c) == NULL) {
        size_t len = strlen((char *)op->matchCount);
        op->matchCount[len] = c;
        op->matchCount[len + 1] = '\0';
    }
}
static int testCode(int regs[], int after[], unsigned char ins[], OP ops[], int opCount) {
    int sum = 0, tmp[4];
    for (int i = 0; i < opCount; i++) {
        memcpy(tmp, regs, sizeof(int) * 4);
        runOp(&ops[i], regs, ins, tmp);
        if (match(after, tmp, 4)) {
            addMatch(&ops[i], ins[0]);
            sum++;
        }
    }
    return sum;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [content componentsSeparatedByString:@"\n"];
        OP ops[] = {
            {'r','r','+',"addr"},
            {'r','v','+',"addi"},
            {'r','r','*',"mulr"},
            {'r','v','*',"muli"},
            {'r','r','&',"banr"},
            {'r','v','&',"bani"},
            {'r','r','|',"borr"},
            {'r','v','|',"bori"},
            {'r','r','a',"setr"},
            {'v','r','a',"seti"},
            {'v','r','>',"gtir"},
            {'r','v','>',"gtri"},
            {'r','r','>',"gtrr"},
            {'v','r','=',"eqir"},
            {'r','v','=',"eqri"},
            {'r','r','=',"eqir"}
        };
        int opCount = sizeof(ops) / sizeof(ops[0]);
        for (int i = 0; i < opCount; i++) ops[i].matchCount[0] = '\0';
        int total = 0;
        for (NSUInteger i = 0; i < lines.count; ) {
            NSString *line = lines[i];
            if ([line hasPrefix:@"Before:"]) {
                int regs[4], after[4];
                unsigned char ins[4];
                sscanf(line.UTF8String, "Before: [%d, %d, %d, %d]", &regs[0], &regs[1], &regs[2], &regs[3]);
                i++;
                sscanf(lines[i].UTF8String, "%hhu %hhu %hhu %hhu", &ins[0], &ins[1], &ins[2], &ins[3]);
                i++;
                sscanf(lines[i].UTF8String, "After: [%d, %d, %d, %d]", &after[0], &after[1], &after[2], &after[3]);
                i++;
                if (testCode(regs, after, ins, ops, opCount) >= 3) total++;
                while (i < lines.count && lines[i].length == 0) i++;
            } else {
                i++;
            }
        }
        printf("%d\n", total);
    }
    return 0;
}
