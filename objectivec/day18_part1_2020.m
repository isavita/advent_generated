#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path
                                                    encoding:NSUTF8StringEncoding
                                                       error:nil];
        NSArray *lines = [content componentsSeparatedByString:@"\n"];
        __block long long sum = 0;
        
        for (NSString *line in lines) {
            if ([line length] == 0) continue;
            
            NSMutableArray<NSNumber *> *ops = [NSMutableArray array];
            NSMutableArray<NSNumber *> *vals = [NSMutableArray array];
            
            NSUInteger i = 0;
            NSUInteger len = [line length];
            while (i < len) {
                unichar c = [line characterAtIndex:i];
                if (c == ' ') { i++; continue; }
                if (c == '(') {
                    [ops addObject:@(c)];
                    i++;
                } else if (c == '+' || c == '*') {
                    while ([ops count] && [[ops lastObject] charValue] != '(') {
                        char op = [[ops lastObject] charValue]; [ops removeLastObject];
                        long long b = [[vals lastObject] longLongValue]; [vals removeLastObject];
                        long long a = [[vals lastObject] longLongValue]; [vals removeLastObject];
                        long long res = (op == '+') ? (a + b) : (a * b);
                        [vals addObject:@(res)];
                    }
                    [ops addObject:@(c)];
                    i++;
                } else if (c == ')') {
                    while ([[ops lastObject] charValue] != '(') {
                        char op = [[ops lastObject] charValue]; [ops removeLastObject];
                        long long b = [[vals lastObject] longLongValue]; [vals removeLastObject];
                        long long a = [[vals lastObject] longLongValue]; [vals removeLastObject];
                        long long res = (op == '+') ? (a + b) : (a * b);
                        [vals addObject:@(res)];
                    }
                    [ops removeLastObject];
                    i++;
                } else if (c >= '0' && c <= '9') {
                    long long num = 0;
                    while (i < len) {
                        unichar d = [line characterAtIndex:i];
                        if (d < '0' || d > '9') break;
                        num = num * 10 + (d - '0');
                        i++;
                    }
                    [vals addObject:@(num)];
                } else {
                    i++;
                }
            }
            while ([ops count]) {
                char op = [[ops lastObject] charValue]; [ops removeLastObject];
                long long b = [[vals lastObject] longLongValue]; [vals removeLastObject];
                long long a = [[vals lastObject] longLongValue]; [vals removeLastObject];
                long long res = (op == '+') ? (a + b) : (a * b);
                [vals addObject:@(res)];
            }
            sum += [[vals lastObject] longLongValue];
        }
        printf("%lld\n", sum);
    }
    return 0;
}