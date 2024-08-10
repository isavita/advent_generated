#import <Foundation/Foundation.h>

@interface LLNode : NSObject
@property (nonatomic) NSInteger elfNum;
@property (nonatomic) NSInteger presents;
@property (nonatomic, strong) LLNode *next;
@end

@implementation LLNode
@end

NSInteger elephant(NSString *input) {
    NSInteger startingElves = [input integerValue];
    LLNode *root = [LLNode new];
    root.elfNum = 1;
    root.presents = 1;
    
    LLNode *iter = root;
    for (NSInteger i = 2; i <= startingElves; i++) {
        iter.next = [LLNode new];
        iter = iter.next;
        iter.elfNum = i;
        iter.presents = 1;
    }
    iter.next = root;

    BOOL isOddLength = startingElves % 2 == 1;
    LLNode *beforeAcross = root;
    for (NSInteger i = 0; i < startingElves / 2 - 1; i++) {
        beforeAcross = beforeAcross.next;
    }

    while (root.next != root) {
        root.presents += beforeAcross.next.presents;
        beforeAcross.next = beforeAcross.next.next;
        if (isOddLength) {
            beforeAcross = beforeAcross.next;
        }
        isOddLength = !isOddLength;
        root = root.next;
    }

    return root.elfNum;
}

NSString *readFile(NSString *filePath) {
    NSString *absolutePath = [NSString stringWithFormat:@"%@/%@", [[NSFileManager defaultManager] currentDirectoryPath], filePath];
    NSError *error;
    NSString *content = [NSString stringWithContentsOfFile:absolutePath encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        @throw [NSException exceptionWithName:@"FileReadError" reason:error.localizedDescription userInfo:nil];
    }
    return [content stringByTrimmingCharactersInSet:[NSCharacterSet newlineCharacterSet]];
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *input = readFile(@"input.txt");
        NSInteger ans = elephant(input);
        NSLog(@"%ld", (long)ans);
    }
    return 0;
}