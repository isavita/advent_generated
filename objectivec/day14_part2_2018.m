#import <Foundation/Foundation.h>

@interface AppDelegate : NSObject

- (void)run;

@end

@implementation AppDelegate

- (void)run {
    NSError *error;
    NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
    if (!input) {
        NSLog(@"Error reading input file: %@", error);
        return;
    }

    NSMutableArray *scoreboard = [NSMutableArray arrayWithObjects:@3, @7, nil];
    NSUInteger elf1 = 0, elf2 = 1;
    NSUInteger inputLen = input.length;
    NSMutableArray *inputSequence = [NSMutableArray arrayWithCapacity:inputLen];

    for (NSUInteger i = 0; i < inputLen; i++) {
        [inputSequence addObject:@([input characterAtIndex:i] - '0')];
    }

    while (1) {
        NSUInteger newScore = [scoreboard[elf1] unsignedIntegerValue] + [scoreboard[elf2] unsignedIntegerValue];
        if (newScore >= 10) {
            [scoreboard addObject:@(newScore / 10)];
            if ([self checkSequence:scoreboard sequence:inputSequence]) {
                break;
            }
        }
        [scoreboard addObject:@(newScore % 10)];
        if ([self checkSequence:scoreboard sequence:inputSequence]) {
            break;
        }

        elf1 = (elf1 + [scoreboard[elf1] unsignedIntegerValue] + 1) % scoreboard.count;
        elf2 = (elf2 + [scoreboard[elf2] unsignedIntegerValue] + 1) % scoreboard.count;
    }

    NSLog(@"%lu", (unsigned long)scoreboard.count - inputLen);
}

- (BOOL)checkSequence:(NSMutableArray *)scoreboard sequence:(NSMutableArray *)sequence {
    if (scoreboard.count < sequence.count) {
        return NO;
    }
    NSUInteger start = scoreboard.count - sequence.count;
    for (NSUInteger i = 0; i < sequence.count; i++) {
        if ([scoreboard[start + i] unsignedIntegerValue] != [sequence[i] unsignedIntegerValue]) {
            return NO;
        }
    }
    return YES;
}

@end

int main(int argc, char *argv[]) {
    @autoreleasepool {
        AppDelegate *appDelegate = [[AppDelegate alloc] init];
        [appDelegate run];
    }
    return 0;
}