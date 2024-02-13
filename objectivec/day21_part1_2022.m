#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        NSArray *lines = [input componentsSeparatedByString:@"\n"];
        
        NSMutableDictionary *monkeyJobs = [NSMutableDictionary dictionary];
        NSMutableDictionary *monkeyNumbers = [NSMutableDictionary dictionary];
        
        for (NSString *line in lines) {
            NSArray *components = [line componentsSeparatedByString:@": "];
            NSString *monkey = components[0];
            NSString *job = components[1];
            monkeyJobs[monkey] = job;
        }
        
        while (![monkeyNumbers objectForKey:@"root"]) {
            for (NSString *monkey in monkeyJobs.allKeys) {
                NSString *job = monkeyJobs[monkey];
                NSArray *operands = [job componentsSeparatedByString:@" "];
                if ([operands count] == 1) {
                    monkeyNumbers[monkey] = operands[0];
                } else {
                    NSString *operator = operands[1];
                    if ([monkeyNumbers objectForKey:operands[0]] && [monkeyNumbers objectForKey:operands[2]]) {
                        NSInteger num1 = [monkeyNumbers[operands[0]] integerValue];
                        NSInteger num2 = [monkeyNumbers[operands[2]] integerValue];
                        if ([operator isEqualToString:@"+"]) {
                            monkeyNumbers[monkey] = [NSString stringWithFormat:@"%ld", num1 + num2];
                        } else if ([operator isEqualToString:@"-"]) {
                            monkeyNumbers[monkey] = [NSString stringWithFormat:@"%ld", num1 - num2];
                        } else if ([operator isEqualToString:@"*"]) {
                            monkeyNumbers[monkey] = [NSString stringWithFormat:@"%ld", num1 * num2];
                        } else if ([operator isEqualToString:@"/"]) {
                            monkeyNumbers[monkey] = [NSString stringWithFormat:@"%ld", num1 / num2];
                        }
                    }
                }
            }
        }
        
        printf("%s\n", [monkeyNumbers[@"root"] UTF8String]);
    }
    return 0;
}