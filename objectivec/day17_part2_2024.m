
#import <Foundation/Foundation.h>
#import <stdio.h>
#import <stdlib.h>
#import <string.h>
#import <stdint.h>
#import <stdbool.h>

typedef struct {
    int64_t a;
    int64_t b;
    int64_t c;
    int *program;
    int program_len;
} Program;

static inline int64_t computeOperand(int v, int64_t a, int64_t b, int64_t c) {
    return (v <= 3) ? v : (v == 4 ? a : (v == 5 ? b : (v == 6 ? c : 0)));
}

int *simulateComputer(Program p, int *out_len) {
    int *outs = NULL, outs_cap = 0;
    *out_len = 0;
    int64_t a = p.a, b = p.b, c = p.c;
    int *in = p.program, n = p.program_len;
    for (int i = 0; i < n; i += 2) {
        int cmd = in[i];
        switch (cmd) {
            case 0: a >>= computeOperand(in[i+1], a, b, c); break;
            case 1: b ^= in[i+1]; break;
            case 2: b = computeOperand(in[i+1], a, b, c) % 8; break;
            case 3: if (a) i = in[i+1] - 2; break;
            case 4: b ^= c; break;
            case 5: {
                if (*out_len == outs_cap) outs = realloc(outs, (outs_cap = outs_cap ? outs_cap*2 : 1) * sizeof(int));
                outs[(*out_len)++] = computeOperand(in[i+1], a, b, c) % 8;
                break;
            }
            case 6: b = a >> computeOperand(in[i+1], a, b, c); break;
            case 7: c = a >> computeOperand(in[i+1], a, b, c); break;
        }
    }
    return outs;
}

typedef struct { int a; int64_t b; } Pair;

int comparePairs(const void *x, const void *y) {
    const Pair *a = x, *b = y;
    if (a->a != b->a) return a->a - b->a;
    return (a->b < b->b) ? -1 : (a->b > b->b);
}

int64_t *check(Program p, int *valid_len) {
    int64_t *valids = NULL, val_cap = 0;
    *valid_len = 0;
    Pair *stack = NULL, *seen = NULL;
    int stack_sz = 0, stack_cap = 0, seen_sz = 0, seen_cap = 0;
    stack = malloc(sizeof(Pair));
    stack[0] = (Pair){0,0};
    stack_sz = 1; stack_cap = 1;
    while (stack_sz) {
        Pair cur = stack[--stack_sz];
        bool dup = false;
        for (int i=0;i<seen_sz;i++) if (seen[i].a==cur.a && seen[i].b==cur.b) { dup = true; break; }
        if (dup) continue;
        if (seen_sz == seen_cap) seen = realloc(seen, (seen_cap = seen_cap?seen_cap*2:1)*sizeof(Pair));
        seen[seen_sz++] = cur;
        if (cur.a == p.program_len) {
            if (*valid_len == val_cap) valids = realloc(valids, (val_cap = val_cap?val_cap*2:1)*sizeof(int64_t));
            valids[(*valid_len)++] = cur.b;
        } else {
            for (int64_t i=0;i<8;i++) {
                int64_t newScore = i + 8*cur.b;
                Program tp = {newScore, p.b, p.c, p.program, p.program_len};
                int outlen;
                int *res = simulateComputer(tp, &outlen);
                if (outlen && res[0]==p.program[p.program_len-1-cur.a]) {
                    if (stack_sz == stack_cap) stack = realloc(stack, (stack_cap = stack_cap?stack_cap*2:1)*sizeof(Pair));
                    stack[stack_sz++] = (Pair){cur.a+1, newScore};
                }
                free(res);
            }
        }
    }
    free(stack);
    free(seen);
    return valids;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString*> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        int64_t a=0,b=0,c=0;
        int *program = NULL, prog_len=0, prog_cap=0;
        for (NSString *ln in lines) {
            NSArray<NSString*> *parts = [ln componentsSeparatedByString:@":"];
            if (parts.count<2) continue;
            NSString *key = parts[0];
            NSString *val = parts[1];
            if ([key isEqualToString:@"Register A"]) a = [val longLongValue];
            else if ([key isEqualToString:@"Register B"]) b = [val longLongValue];
            else if ([key isEqualToString:@"Register C"]) c = [val longLongValue];
            else if ([key isEqualToString:@"Program"]) {
                NSArray<NSString*> *nums = [val componentsSeparatedByString:@","];
                for (NSString *n in nums) {
                    if (prog_len==prog_cap) program = realloc(program, (prog_cap = prog_cap?prog_cap*2:1)*sizeof(int));
                    program[prog_len++] = [n intValue];
                }
            }
        }
        Program p = {a,b,c,program,prog_len};
        int valid_len;
        int64_t *vals = check(p, &valid_len);
        if (valid_len) {
            int64_t min = vals[0];
            for (int i=1;i<valid_len;i++) if (vals[i]<min) min=vals[i];
            printf("%lld\n", min);
        } else {
            printf("No valid values found\n");
        }
        free(vals);
        free(program);
    }
    return 0;
}
