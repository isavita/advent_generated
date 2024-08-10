#import <Foundation/Foundation.h>

@interface SyntaxScorer : NSObject
- (instancetype)initWithFilePath:(NSString *)filePath;
- (void)calculateSyntaxScores;
@end

@implementation SyntaxScorer {
    NSString *filePath;
}

- (instancetype)initWithFilePath:(NSString *)filePath {
    self = [super init];
    if (self) {
        self->filePath = filePath;
    }
    return self;
}

- (void)calculateSyntaxScores {
    NSString *content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    NSArray *lines = [content componentsSeparatedByString:@"\n"];
    NSMutableArray *incompleteScores = [NSMutableArray array];
    int syntaxErrorScore = 0;

    for (NSString *line in lines) {
        NSMutableArray *stack = [NSMutableArray array];
        BOOL corrupted = NO;

        for (NSUInteger i = 0; i < line.length; i++) {
            unichar c = [line characterAtIndex:i];
            if (c == '(' || c == '[' || c == '{' || c == '<') {
                [stack addObject:[NSNumber numberWithChar:c]];
            } else {
                if ([stack count] == 0) {
                    corrupted = YES;
                    break;
                }
                char last = [[stack lastObject] charValue];
                [stack removeLastObject];
                if ((c == ')' && last != '(') ||
                    (c == ']' && last != '[') ||
                    (c == '}' && last != '{') ||
                    (c == '>' && last != '<')) {
                    corrupted = YES;
                    syntaxErrorScore += [self scoreForIllegalCharacter:c];
                    break;
                }
            }
        }

        if (!corrupted && [stack count] > 0) {
            long long score = 0;
            for (NSNumber *num in [stack reverseObjectEnumerator]) {
                score = score * 5 + [self scoreForCompletionCharacter:[num charValue]];
            }
            [incompleteScores addObject:@(score)];
        }
    }

    NSLog(@"Total syntax error score: %d", syntaxErrorScore);

    NSArray *sortedScores = [incompleteScores sortedArrayUsingSelector:@selector(compare:)];
    NSUInteger middleIndex = sortedScores.count / 2;
    NSLog(@"Middle score: %lld", [sortedScores[middleIndex] longLongValue]);
}

- (int)scoreForIllegalCharacter:(unichar)c {
    switch (c) {
        case ')': return 3;
        case ']': return 57;
        case '}': return 1197;
        case '>': return 25137;
        default: return 0;
    }
}

- (int)scoreForCompletionCharacter:(unichar)c {
    switch (c) {
        case '(': return 1;
        case '[': return 2;
        case '{': return 3;
        case '<': return 4;
        default: return 0;
    }
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        SyntaxScorer *scorer = [[SyntaxScorer alloc] initWithFilePath:@"input.txt"];
        [scorer calculateSyntaxScores];
    }
    return 0;
}