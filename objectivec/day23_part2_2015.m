
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:nil];
        if (!content) return 1;
        NSArray<NSString *> *lines = [content componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
        NSMutableArray<NSString *> *instructions = [NSMutableArray array];
        for (NSString *line in lines) {
            NSString *trim = [line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
            if (trim.length) [instructions addObject:trim];
        }
        unsigned long long regA = 1, regB = 0;
        NSInteger ip = 0, count = instructions.count;
        while (ip >= 0 && ip < count) {
            NSString *line = instructions[ip];
            NSString *op = [line substringToIndex:3];
            if ([op isEqualToString:@"hlf"]) {
                char r = [line characterAtIndex:4];
                if (r == 'a') regA >>= 1; else regB >>= 1;
                ip++;
            } else if ([op isEqualToString:@"tpl"]) {
                char r = [line characterAtIndex:4];
                if (r == 'a') regA *= 3; else regB *= 3;
                ip++;
            } else if ([op isEqualToString:@"inc"]) {
                char r = [line characterAtIndex:4];
                if (r == 'a') regA++; else regB++;
                ip++;
            } else if ([op isEqualToString:@"jmp"]) {
                NSInteger offset = [[line substringFromIndex:4] integerValue];
                ip += offset;
            } else if ([op isEqualToString:@"jie"]) {
                char r = [line characterAtIndex:4];
                NSInteger offset = [[line substringFromIndex:7] integerValue];
                unsigned long long val = (r == 'a') ? regA : regB;
                ip = (val % 2 == 0) ? ip + offset : ip + 1;
            } else if ([op isEqualToString:@"jio"]) {
                char r = [line characterAtIndex:4];
                NSInteger offset = [[line substringFromIndex:7] integerValue];
                unsigned long long val = (r == 'a') ? regA : regB;
                ip = (val == 1) ? ip + offset : ip + 1;
            } else {
                return 1;
            }
        }
        printf("%llu\n", regB);
    }
    return 0;
}
