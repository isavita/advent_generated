
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define MAX_STACK_SIZE 256

static inline void push_num(long long *stack, int *top, long long val) {
    stack[++(*top)] = val;
}
static inline long long pop_num(long long *stack, int *top) {
    return stack[(*top)--];
}
static inline void push_op(char *stack, int *top, char op) {
    stack[++(*top)] = op;
}
static inline char pop_op(char *stack, int *top) {
    return stack[(*top)--];
}
static inline char peek_op(char *stack, int top) {
    return top >= 0 ? stack[top] : '\0';
}
static inline int precedence(char op, int rule) {
    if (op == '(') return 0;
    return (rule == 1) ? 1 : (op == '+' ? 2 : 1);
}
static inline void apply_op(long long *numStack, int *numTop, char *opStack, int *opTop) {
    char op = pop_op(opStack, opTop);
    long long r = pop_num(numStack, numTop);
    long long l = pop_num(numStack, numTop);
    push_num(numStack, numTop, op == '+' ? l + r : l * r);
}
static long long evaluate(const char *expr, int rule) {
    long long numStack[MAX_STACK_SIZE];
    char opStack[MAX_STACK_SIZE];
    int numTop = -1, opTop = -1;
    const char *p = expr;
    while (*p) {
        if (isspace((unsigned char)*p)) { p++; }
        else if (isdigit((unsigned char)*p)) {
            char *end;
            long long v = strtoll(p, &end, 10);
            push_num(numStack, &numTop, v);
            p = end;
        } else if (*p == '(') {
            push_op(opStack, &opTop, '(');
            p++;
        } else if (*p == ')') {
            while (peek_op(opStack, opTop) != '(') apply_op(numStack, &numTop, opStack, &opTop);
            pop_op(opStack, &opTop);
            p++;
        } else { // + or *
            while (opTop >= 0 && peek_op(opStack, opTop) != '(' &&
                   precedence(peek_op(opStack, opTop), rule) >= precedence(*p, rule))
                apply_op(numStack, &numTop, opStack, &opTop);
            push_op(opStack, &opTop, *p);
            p++;
        }
    }
    while (opTop >= 0) apply_op(numStack, &numTop, opStack, &opTop);
    return pop_num(numStack, &numTop);
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSFileHandle *fh = [NSFileHandle fileHandleForReadingAtPath:path];
        if (!fh) return 1;
        NSData *data = [fh readDataToEndOfFile];
        NSString *content = [[NSString alloc] initWithData:data encoding:NSUTF8StringEncoding];
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        long long total1 = 0, total2 = 0;
        for (NSString *line in lines) {
            if (line.length == 0) continue;
            const char *c = [line UTF8String];
            total1 += evaluate(c, 1);
            total2 += evaluate(c, 2);
        }
        printf("%lld\n%lld\n", total1, total2);
    }
    return 0;
}
