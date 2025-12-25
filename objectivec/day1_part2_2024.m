
#import <Foundation/Foundation.h>

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *path = @"input.txt";
        NSString *content = [NSString stringWithContentsOfFile:path
                                                   encoding:NSUTF8StringEncoding
                                                      error:nil];
        if (!content) {
            NSLog(@"0");
            return 0;
        }

        NSArray *lines = [content componentsSeparatedByCharactersInSet:
                         [NSCharacterSet newlineCharacterSet]];
        NSMutableArray *left  = [NSMutableArray array];
        NSMutableArray *right = [NSMutableArray array];

        for (NSString *line in lines) {
            NSArray *parts = [line componentsSeparatedByString:@" "];
            if (parts.count < 2) continue;
            NSString *l = parts[0];
            NSString *r = parts[parts.count-1];
            [left  addObject:l];
            [right addObject:r];
        }

        NSCountedSet *counts = [NSCountedSet setWithArray:right];
        long score = 0;
        for (NSString *l in left) {
            NSInteger n = [l integerValue];
            score += n * [counts countForObject:l];
        }
        printf("%ld\n", score);
    }
    return 0;
}
