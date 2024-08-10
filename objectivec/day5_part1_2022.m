#import <Foundation/Foundation.h>

void parseStacks(NSMutableArray<NSMutableArray<NSString *> *> *stacks, NSArray<NSString *> *lines) {
    for (NSString *line in lines) {
        if ([line containsString:@"1"]) break;
        for (NSUInteger i = 0; i < [line length]; i += 4) {
            NSString *crate = [line substringWithRange:NSMakeRange(i + 1, 1)];
            if (![crate isEqualToString:@" "]) {
                NSUInteger stackIndex = i / 4;
                while (stacks.count <= stackIndex) {
                    [stacks addObject:[NSMutableArray array]];
                }
                [stacks[stackIndex] insertObject:crate atIndex:0];
            }
        }
    }
}

void parseMoves(NSMutableArray<NSDictionary *> *moves, NSArray<NSString *> *lines) {
    NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"move (\\d+) from (\\d+) to (\\d+)" options:0 error:nil];
    for (NSString *line in lines) {
        NSTextCheckingResult *match = [regex firstMatchInString:line options:0 range:NSMakeRange(0, [line length])];
        if (match) {
            NSString *quantity = [line substringWithRange:[match rangeAtIndex:1]];
            NSString *from = [line substringWithRange:[match rangeAtIndex:2]];
            NSString *to = [line substringWithRange:[match rangeAtIndex:3]];
            [moves addObject:@{@"quantity": quantity, @"from": from, @"to": to}];
        }
    }
}

void applyMoves(NSMutableArray<NSMutableArray<NSString *> *> *stacks, NSArray<NSDictionary *> *moves) {
    for (NSDictionary *move in moves) {
        NSInteger quantity = [move[@"quantity"] integerValue];
        NSInteger from = [move[@"from"] integerValue] - 1;
        NSInteger to = [move[@"to"] integerValue] - 1;

        NSMutableArray<NSString *> *fromStack = stacks[from];
        NSMutableArray<NSString *> *toStack = stacks[to];

        for (NSInteger i = 0; i < quantity; i++) {
            NSString *crate = fromStack.lastObject;
            [fromStack removeLastObject];
            [toStack addObject:crate];
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *filePath = @"input.txt";
        NSError *error = nil;
        NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            NSLog(@"Error reading file: %@", error);
            return 1;
        }

        NSArray<NSString *> *lines = [fileContents componentsSeparatedByString:@"\n"];
        NSMutableArray<NSMutableArray<NSString *> *> *stacks = [NSMutableArray array];
        NSMutableArray<NSDictionary *> *moves = [NSMutableArray array];

        BOOL parsingStacks = YES;
        for (NSString *line in lines) {
            if ([line isEqualToString:@""]) {
                parsingStacks = NO;
                continue;
            }
            if (parsingStacks) {
                parseStacks(stacks, @[line]);
            } else {
                parseMoves(moves, @[line]);
            }
        }

        applyMoves(stacks, moves);

        NSMutableString *result = [NSMutableString string];
        for (NSMutableArray<NSString *> *stack in stacks) {
            [result appendString:stack.lastObject];
        }

        NSLog(@"%@", result);
    }
    return 0;
}