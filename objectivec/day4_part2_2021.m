#import <Foundation/Foundation.h>

@interface BoardState : NSObject
@property (nonatomic, strong) NSArray<NSArray<NSNumber *> *> *board;
@property (nonatomic, strong) NSMutableArray<NSMutableArray<NSNumber *> *> *picked;

- (instancetype)initWithBoard:(NSArray<NSArray<NSNumber *> *> *)board;
- (BOOL)pickNum:(NSNumber *)num;
- (NSInteger)score;
@end

@implementation BoardState

- (instancetype)initWithBoard:(NSArray<NSArray<NSNumber *> *> *)board {
    self = [super init];
    if (self) {
        _board = board;
        _picked = [NSMutableArray arrayWithCapacity:board.count];
        for (NSUInteger i = 0; i < board.count; i++) {
            [_picked addObject:[NSMutableArray arrayWithCapacity:board[i].count]];
            for (NSUInteger j = 0; j < board[i].count; j++) {
                [_picked[i] addObject:@(NO)];
            }
        }
    }
    return self;
}

- (BOOL)pickNum:(NSNumber *)num {
    for (NSUInteger r = 0; r < self.board.count; r++) {
        for (NSUInteger c = 0; c < self.board[r].count; c++) {
            if ([self.board[r][c] isEqual:num]) {
                self.picked[r][c] = @(YES);
            }
        }
    }
    
    for (NSUInteger i = 0; i < self.board.count; i++) {
        BOOL isFullRow = YES, isFullCol = YES;
        for (NSUInteger j = 0; j < self.board.count; j++) {
            if (![self.picked[i][j] boolValue]) isFullRow = NO;
            if (![self.picked[j][i] boolValue]) isFullCol = NO;
        }
        if (isFullRow || isFullCol) return YES;
    }
    return NO;
}

- (NSInteger)score {
    NSInteger score = 0;
    for (NSUInteger r = 0; r < self.board.count; r++) {
        for (NSUInteger c = 0; c < self.board[r].count; c++) {
            if (![self.picked[r][c] boolValue]) {
                score += [self.board[r][c] integerValue];
            }
        }
    }
    return score;
}
@end

NSArray<NSNumber *> *parseInput(NSString *input, NSMutableArray<BoardState *> *boards) {
    NSArray<NSString *> *lines = [input componentsSeparatedByString:@"\n\n"];
    NSArray<NSString *> *numsStr = [lines[0] componentsSeparatedByString:@","];
    NSMutableArray<NSNumber *> *nums = [NSMutableArray arrayWithCapacity:numsStr.count];
    
    for (NSString *s in numsStr) {
        [nums addObject:@(s.integerValue)];
    }
    
    for (NSString *grid in [lines subarrayWithRange:NSMakeRange(1, lines.count - 1)]) {
        NSMutableArray<NSMutableArray<NSNumber *> *> *b = [NSMutableArray array];
        NSArray<NSString *> *gridLines = [grid componentsSeparatedByString:@"\n"];
        
        for (NSString *line in gridLines) {
            NSArray<NSString *> *parts = [[line stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]] componentsSeparatedByCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
            NSMutableArray<NSNumber *> *row = [NSMutableArray array];
            for (NSString *p in parts) {
                [row addObject:@(p.integerValue)];
            }
            [b addObject:row];
        }
        
        [boards addObject:[[BoardState alloc] initWithBoard:b]];
    }
    
    return nums;
}

NSInteger solve(NSArray<NSNumber *> *nums, NSArray<BoardState *> *boards) {
    NSInteger lastWinningScore = -1;
    NSMutableSet<NSNumber *> *alreadyWon = [NSMutableSet set];
    
    for (NSNumber *n in nums) {
        for (NSUInteger bi = 0; bi < boards.count; bi++) {
            if ([alreadyWon containsObject:@(bi)]) continue;
            if ([boards[bi] pickNum:n]) {
                lastWinningScore = [boards[bi] score] * n.integerValue;
                [alreadyWon addObject:@(bi)];
            }
        }
    }
    return lastWinningScore;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSError *error = nil;
        NSString *input = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:&error];
        if (error) {
            return -1;
        }
        
        NSMutableArray<BoardState *> *boards = [NSMutableArray array];
        NSArray<NSNumber *> *nums = parseInput(input, boards);
        NSInteger result = solve(nums, boards);
        NSLog(@"%ld", (long)result);
    }
    return 0;
}