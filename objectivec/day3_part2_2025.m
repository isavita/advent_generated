
#import <Foundation/Foundation.h>

#define TARGET 12

static void addBig(NSMutableString *sum, NSString *num) {
    NSInteger i = 0, carry = 0;
    NSInteger al = sum.length, bl = num.length;
    NSMutableString *res = [NSMutableString string];
    while (i < al || i < bl || carry) {
        NSInteger d1 = i < al ? [sum characterAtIndex:al - 1 - i] - '0' : 0;
        NSInteger d2 = i < bl ? [num characterAtIndex:bl - 1 - i] - '0' : 0;
        NSInteger t = d1 + d2 + carry;
        carry = t / 10;
        [res appendFormat:@"%c", (char)('0' + (t % 10))];
        i++;
    }
    // reverse result
    NSUInteger len = res.length;
    for (NSUInteger j = 0; j < len / 2; j++) {
        unichar a = [res characterAtIndex:j];
        unichar b = [res characterAtIndex:len - 1 - j];
        [res replaceCharactersInRange:NSMakeRange(j, 1) withString:[NSString stringWithCharacters:&b length:1]];
        [res replaceCharactersInRange:NSMakeRange(len - 1 - j, 1) withString:[NSString stringWithCharacters:&a length:1]];
    }
    [sum setString:res];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSError *err = nil;
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:&err];
        if (!content) return 1;
        NSMutableString *total = [NSMutableString stringWithString:@"0"];
        [content enumerateLinesUsingBlock:^(NSString *line, BOOL *stop) {
            NSString *buf = [line stringByTrimmingCharactersInSet:
                             [NSCharacterSet characterSetWithCharactersInString:@"0123456789"].invertedSet];
            if (buf.length < TARGET) return;
            NSInteger rem = buf.length - TARGET;
            NSMutableString *stack = [NSMutableString string];
            for (NSUInteger i = 0; i < buf.length; i++) {
                unichar c = [buf characterAtIndex:i];
                while (rem && stack.length && [stack characterAtIndex:stack.length-1] < c) {
                    [stack deleteCharactersInRange:NSMakeRange(stack.length-1,1)];
                    rem--;
                }
                [stack appendFormat:@"%c", c];
            }
            if (stack.length > TARGET) [stack deleteCharactersInRange:NSMakeRange(TARGET, stack.length-TARGET)];
            addBig(total, stack);
        }];
        printf("%s\n", total.UTF8String);
    }
    return 0;
}
